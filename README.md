# Objetos2-TP3 
## REGISTRO DE BAD SMELLS Y REFACTORING
#### *Bad smell*: Duplicated Code 

Encontramos que las clases **Answer** y **Question** tienen campos en común (variables de instancia: *timestamp, description, user* y *votes*) y también ambas clases responden a los mensajes *#addVote:, #positiveVotes, #negativeVotes*, con idéntica funcionalidad.

<pre>
Object subclass: #Question
	instanceVariableNames: 'title answers topics timestamp user votes description'
	classVariableNames: ''
	package: 'TP-Refactoring-Model'
</pre>

<pre>
Object subclass: #Answer
	instanceVariableNames: 'question timestamp user votes description'
	classVariableNames: ''
	package: 'TP-Refactoring-Model'
</pre>
>En las definiciones de clase se observan atributos en común

También se observa redundancia en los métodos:
<pre>
 Question>>negativeVotes
	| r |
	r := OrderedCollection new.
	votes do:[:vote | vote isLike ifFalse:[r add: vote]].
	^r
</pre>

<pre>
Answer>>negativeVotes
	| r |
	r := OrderedCollection new.
	votes do:[:vote | vote isLike ifFalse:[r add: vote]].
	^r
</pre>

<pre>
Question>>positiveVotes
	| r |
	r := OrderedCollection new.
	votes do:[:vote | vote isLike ifTrue:[r add: vote]].
	^r
</pre>

<pre>
Answer>>positiveVotes
	| r |
	r := OrderedCollection new.
	votes do:[:vote | vote isLike ifTrue:[r add: vote]].
	^r
</pre>

>**Nota**: Ademas, alguien me puede explicar porque se llama  **| r |** la variable? Poco expresivo.. 

<pre>
Question>>addVote: aVote
	votes add: aVote
</pre>

<pre>
Answer>>addVote: aVote
	votes add: aVote
</pre>

Y por supuesto, este *Bad Smell* también se presenta en los mensajes *>>initialize* de las clases **Question** y **Answer**:
<pre>
Question>>initialize
	answers := OrderedCollection new.
	topics := OrderedCollection new.
	votes := OrderedCollection new.
	timestamp := DateAndTime now.
</pre>

<pre>
Answer>>initialize
	votes := OrderedCollection new.
	timestamp := DateAndTime now.	
</pre>
>Como se puede observar en estos 2 últimos *snippets*, los mensajes *>>initialize* también presentan redundancia.

Para solucionar dicho *Bad smell* se aplicaron los siguientes *Refactorings*:

1. *Refactoring*: **Extract Superclass** y **Pull up Fields**

Lo primero es crear una nueva clase abstracta ***Publication***, la cual tendrá como variables de instancia los campos que **Question** y **Answer** tienen en común:

<pre>
Object subclass: #Publication
	instanceVariableNames: 'timestamp user votes description'
	classVariableNames: ''
</pre>

Las clases **Question** y **Answer** quedarían conformadas de la siguiente manera:

<pre>
Publication subclass: #Question
	instanceVariableNames: 'title answers topics'
	classVariableNames: ''
</pre>
<pre>
Publication subclass: #Answer
	instanceVariableNames: 'question'
	classVariableNames: ''
</pre>

>Se puede apreciar que ambas clases heredan ahora de ***Publication***, y se "subieron" los atributos que **Question** y **Answer** tenían en común.

2. *Refactoring*: **Pull Up Method**

Los métodos idénticos que fueron encontrados en **Question** y **Answer** ahora son llevados a la superclase:

<pre>
Publication>>negativeVotes
	| r |
	r := OrderedCollection new.
	votes do:[:vote | vote isLike ifFalse:[r add: vote]].
	^r
</pre>

<pre>
Publication>>positiveVotes
	| r |
	r := OrderedCollection new.
	votes do:[:vote | vote isLike ifTrue:[r add: vote]].
	^r
</pre>

<pre>
Publication>>addVote: aVote
	votes add: aVote
</pre>
>Los métodos idénticos ahora se "subieron" a **Publication** y se eliminaron de **Question** y **Answer**. Además tambien se promocionaron los *accesors* que tenían en común dichas clases.

3. *Refactoring*: **Pull Up Constructor Body**

Este refactoring no se sigue al pie de la letra, debido a particularidades de *Smalltalk* pero se podría decir que se aplica para "limpiar" los mensajes *>>initialize* de **Question** y **Answer**.

<pre>
Publication>>initialize
	votes := OrderedCollection new.
	timestamp := DateAndTime now.
</pre>

<pre>
Question>>initialize
	super initialize.
	answers := OrderedCollection new.
	topics := OrderedCollection new.
</pre>
>Se ve como **Question** realiza un *LookUp* hacia la superclase para inicializar por defecto *votes* y *timestamp*, para luego inicializar sus atributos particulares. Por su parte, **Answer** ya no necesita en su propio *>>initialize*.
________________________________________________________

#### *Bad smell*: Reinventando la rueda

Se itera sobre la coleccion votes con '*do:*', cuando puede obtenerse el mismo resultado con otro mensaje ya implementado para colecciones (*select:*):

<pre>
Publication>>negativeVotes
	| r |
	r := OrderedCollection new.
	votes do:[:vote | vote isLike ifFalse:[r add: vote]].
	^r
</pre>

<pre>
publication>>positiveVotes
	| r |
	r := OrderedCollection new.
	votes do:[:vote | vote isLike ifTrue:[r add: vote]].
	^r
</pre>

*Refactoring*: **Substitute Algorithm**.

Se utiliza *select:* en lugar de *do*, eliminando la expresión *ifTrue:[]*, y se retorna el resultado de la operación. Se elimina también la variable local. 

<pre>
Publication>>positiveVotes
	^ votes select: [ :vote | vote isLike ].
</pre>

En el caso de *>>negativeVotes* la solución es muy similar, la única diferencia es que se utiliza el mensaje *reject:*:
<pre>
Publication>>negativeVotes
	^votes reject: [ :vote | vote isLike ].
</pre>

Con este *refactoring* se gana legibilidad y se expresa la intención del código de manera más clara.

____________________________________________________________________

#### *Bad smell*: Switch Statements

Analizando el método *#retrieveQuestions: aUser* de la clase **QuestionRetriever**, observamos que el código consiste principalmente en una secuencia de "if option = #symbol", y en cada caso realiza una serie de operaciones para retornar una colección de instancias de Question (según distintos criterios). Esto se considera una mala práctica en OO, ya que puede obtenerse el mismo funcionamiento aplicando polimorfismo. 

>**Nota:** También consideramos que el método presenta el bad smell **Long Method**, que quedará resuelto luego de aplicar el refactoring para Switch Statements. 

<pre>
QuestionRetriever>>retrieveQuestions: aUser
	| qRet temp followingCol topicsCol newsCol popularTCol averageVotes|
	qRet := OrderedCollection new.
	option = #social ifTrue:[
			followingCol := OrderedCollection new.
			aUser following do:[ :follow | followingCol addAll: follow questions ].
			temp := followingCol asSortedCollection:[ :a :b | a positiveVotes size > b positiveVotes size ].
			qRet := temp last: (100 min: temp size).
		].

	option = #topics ifTrue:[
			topicsCol := OrderedCollection new.
			aUser topics do:[ :topic | topicsCol addAll: topic questions ].
			temp := topicsCol asSortedCollection:[ :a :b | a positiveVotes size > b positiveVotes size ].
			qRet := temp last: (100 min: temp size).
		].
	
	option = #news ifTrue:[
			newsCol := OrderedCollection new.
			cuoora questions do:[:q | (q timestamp asDate = Date today) ifTrue: [newsCol add: q]].
			temp := newsCol asSortedCollection:[ :a :b | a positiveVotes size > b positiveVotes size ].
			qRet := temp last: (100 min: temp size).
		].

	option = #popularToday ifTrue:[
			popularTCol := OrderedCollection new.
			cuoora questions do:[:q | (q timestamp asDate = Date today) ifTrue: [popularTCol add: q]].
			averageVotes := (cuoora questions sum: [:q | q positiveVotes size ]) / popularTCol size.
			temp := (popularTCol select:[:q | q positiveVotes size >= averageVotes ]) asSortedCollection:[ :a :b | a positiveVotes size > b positiveVotes size ].
			qRet := temp last: (100 min: temp size).
		].
	
	^qRet reject:[:q | q user = aUser].
</pre>

1. *Refactoring*: **Replace Type Code With Subclasses**.

Type Code: se utiliza una serie de símbolos (*#social, #topics, #news, #popularToday*) como los valores permitidos de la variable de instancia *-option*. Estos símbolos afectan directamente el comportamiento del método (se evalúa *-option* en los condicionales).

En primer lugar creamos una subclase de **QuestionRetriever** por cada uno de los símbolos mencionados.

<pre>
QuestionRetriever subclass: #SocialQuestionRetriever
	instanceVariableNames: ''
	classVariableNames: ''
</pre>

<pre>
QuestionRetriever subclass: #TopicsQuestionRetriever
	instanceVariableNames: ''
	classVariableNames: ''
</pre>

<pre>
QuestionRetriever subclass: #NewsQuestionRetriever
	instanceVariableNames: ''
	classVariableNames: ''
</pre>

<pre>
QuestionRetriever subclass: #PopularTodayQuestionRetriever
	instanceVariableNames: ''
	classVariableNames: ''
</pre>

2. *Refactoring*: **Replace Conditional With Polymorphism**.

A continuación tomamos el código correspondiente a cada una de las subclases que se encuentra en los condicionales de *QuestionRetriever>>retrieveQuestions: aUser*, y utilizando **Extract Method** junto con **Move Method** implementamos los mensajes en las subclases. Para cada subclase se utilizarán sólo las variables temporales necesarias, y se incluye el retorno del método <code>^qRet reject:[:q | q user = aUser].</code> , que es común para todas las subclases.
Una vez que el método se encuentra redefinido por todas las subclases, modificamos el método en la superclase para que el mismo sea abstracto. (ahora la clase **QuestionRetriever** pasa a ser Abstracta)

<pre>
SocialQuestionRetriever>>retrieveQuestions: aUser
	| qRet temp followingCol |
	
	qRet := OrderedCollection new.
	followingCol := OrderedCollection new.
	aUser following do:[ :follow | followingCol addAll: follow questions ].
	temp := followingCol asSortedCollection:[ :a :b | a positiveVotes size > b positiveVotes size ].
	qRet := temp last: (100 min: temp size).
	
	^qRet reject:[:q | q user = aUser].
</pre>

<pre>
TopicsQuestionRetriever>>retrieveQuestions: aUser
	| qRet temp topicsCol |
	
	qRet := OrderedCollection new.
	topicsCol := OrderedCollection new.
	aUser topics do:[ :topic | topicsCol addAll: topic questions ].
	temp := topicsCol asSortedCollection:[ :a :b | a positiveVotes size > b positiveVotes size ].
	qRet := temp last: (100 min: temp size).
	
	^qRet reject:[:q | q user = aUser].
</pre>

<pre>
NewsQuestionRetriever>>retrieveQuestions: aUser
	| qRet temp newsCol |

	qRet := OrderedCollection new.	
	newsCol := OrderedCollection new.
	cuoora questions do:[:q | (q timestamp asDate = Date today) ifTrue: [newsCol add: q]].
	temp := newsCol asSortedCollection:[ :a :b | a positiveVotes size > b positiveVotes size ].
	qRet := temp last: (100 min: temp size).
	
	^qRet reject:[:q | q user = aUser].
</pre>

<pre>
PopularTodayQuestionRetriever>>retrieveQuestions: aUser
	| qRet temp popularTCol averageVotes |
	
	qRet := OrderedCollection new.
	popularTCol := OrderedCollection new.
	cuoora questions do:[:q | (q timestamp asDate = Date today) ifTrue: [popularTCol add: q]].
	averageVotes := (cuoora questions sum: [:q | q positiveVotes size ]) / popularTCol size.
	temp := (popularTCol select:[:q | q positiveVotes size >= averageVotes ]) asSortedCollection:[ :a :b | a positiveVotes size > b positiveVotes size ].
	qRet := temp last: (100 min: temp size).
	
	^qRet reject:[:q | q user = aUser].
</pre>

<pre>
QuestionRetriever(Abstract)>>retrieveQuestions: aUser
	^ self subclassResponsibility.
</pre>

Es claro que ya no es necesario el uso de los símbolos y condicionales, gracias a la implementación de polimorfismo, y por lo tanto la variable de instancia *-option* que ahora heredan las subclases ya no se utiliza. Tanto la variable como su setter *#option: anOption*, tienen olor a **Dead Code** (por el hecho de que ya no se usan), y en consecuencia *#initialize* y uno de los constructores dejan de tener sentido.

<pre>
QuestionRetriever>>initialize
	option := #social
</pre>

<pre>
QuestionRetriever(Class side)>>new: cuoora and: aSymbol
	^ self new cuoora: cuoora; option:aSymbol; yourself.
</pre>

Para este refactoring simplemente eliminamos la variable *-option* y los métodos mencionados. 
Al ejecutar los tests correspondientes luego del refactoring, encontramos que los test fallaron dado que el constructor eliminado era utilizado en el setUp:

<pre>
QuestionRetrieverTest>>setUp
	(...)
  
   socialRetriever := QuestionRetriever new: cuoora and: #social.
   topicsRetriever := QuestionRetriever new: cuoora and: #topics.
   newsRetriever := QuestionRetriever new: cuoora and: #news.
   popularTodayRetriever := QuestionRetriever new: cuoora and: #popularToday.
</pre>

Para solucionar esto sin modificar la funcionalidad del test, reescribimos el código para que se instancie, en cada caso, un objeto de las subclases de QuestionRetriever (que al ser abstracta sería incorrecto instanciar). Luego comprobamos que los test pasen sin errores.

<pre>
QuestionRetrieverTest>>setUp
	(...)
 
   socialRetriever := SocialQuestionRetriever new: cuoora.
   topicsRetriever := TopicsQuestionRetriever new: cuoora.
   newsRetriever := NewsQuestionRetriever new: cuoora.
   popularTodayRetriever := PopularTodayQuestionRetriever new: cuoora.
</pre>

____________________________________________________________________

#### *Bad smell*: Duplicated Code

En las subclases de **QuestionRetriever** se puede observar que hay código duplicado con algunas particularidades que los diferencian, pero en general los algoritmos siguen los mismos pasos, en el mismo orden:
1. Obtener una colección de instancias de **Question** relevantes para cada subclase (ya sea desde *aUser* o *cuoora*).
2. Ordenar dicha colección en forma ascendente según la cantidad de votos positivos de cada instancia.
3. Filtrar la colección, retornando una colección con las últimas 100 questions que no contenga las questions realizadas por *aUser*.

<pre>
NewsQuestionRetriever>>retrieveQuestions: aUser
	| qRet temp newsCol |
	
	qRet := OrderedCollection new.	
	newsCol := OrderedCollection new.
	cuoora questions do:[:q | (q timestamp asDate = Date today) ifTrue: [newsCol add: q]].
	
	temp := newsCol asSortedCollection:[ :a :b | a positiveVotes size > b positiveVotes size ].
	
	qRet := temp last: (100 min: temp size).
	^qRet reject:[:q | q user = aUser].
</pre>

<pre>
PopularTodayQuestionRetriever>>retrieveQuestions: aUser
	| qRet temp popularTCol averageVotes |
	
	popularTCol := OrderedCollection new.
	cuoora questions do:[:q | (q timestamp asDate = Date today) ifTrue: [popularTCol add: q]].
	
	averageVotes := (cuoora questions sum: [:q | q positiveVotes size ]) / popularTCol size.
	temp := (popularTCol select:[:q | q positiveVotes size >= averageVotes ]) asSortedCollection:[ :a :b | a positiveVotes size > b positiveVotes size ].
	
	qRet := temp last: (100 min: temp size).
	^qRet reject:[:q | q user = aUser].
</pre>

<pre>

SocialQuestionRetriever>>retrieveQuestions: aUser
	| qRet temp followingCol |
	
	followingCol := OrderedCollection new.
	aUser following do:[ :follow | followingCol addAll: follow questions ].
	
	temp := followingCol asSortedCollection:[ :a :b | a positiveVotes size > b positiveVotes size ].
	
	qRet := temp last: (100 min: temp size).
	^qRet reject:[:q | q user = aUser].	
</pre>

<pre>
TopicsQuestionRetriever>>retrieveQuestions: aUser
retrieveQuestions: aUser
	| qRet temp topicsCol |
	
	topicsCol := OrderedCollection new.
	aUser topics do:[ :topic | topicsCol addAll: topic questions ].
	
	temp := topicsCol asSortedCollection:[ :a :b | a positiveVotes size > b positiveVotes size ].
	
	qRet := temp last: (100 min: temp size).
	^qRet reject:[:q | q user = aUser].
</pre>

*Refactoring*: **Form Template Method**

Para la aplicación de este refactoring, en primer lugar hacemos **Extract Method** en todas las subclases para cada uno de los pasos mencionados, con un nombre en común que represente la intención del paso.

Primero se extrae el código repetido para el paso 3 en un nuevo método, quedando conformado de la siguiente manera:

<pre>
retrieveQuestionsFor: aUser from: aCollection
	| qRet |
	qRet := aCollection last: (100 min: aCollection size).
	^qRet reject:[:q | q user = aUser].
</pre>

Este procedimiento se podría realizar en todas las clases anteriormente mencionadas. Al tener todas idéntico funcionamiento, la implementación del mensaje puede estar definida en ***QuestionRetriever***, realizando **Pull Up Method**

<pre>
QuestionRetriever>>retrieveQuestionsFor: aUser from: aCollection
	| qRet |
	qRet := aCollection last: (100 min: aCollection size).
	^qRet reject:[:q | q user = aUser].
</pre>

Finalmente, los mensajes de las clases "hijas" de ***QuestionRetriever*** quedarían implementados de la siguiente manera:

<pre>
NewsQuestionRetriever>>retrieveQuestions: aUser
	| temp newsCol |

	newsCol := OrderedCollection new.
	cuoora questions do:[:q | (q timestamp asDate = Date today) ifTrue: [newsCol add: q]].
	temp := newsCol asSortedCollection:[ :a :b | a positiveVotes size > b positiveVotes size ].
	^ self retrieveQuestionsFor: aUser from: temp. 
</pre>

<pre>
PopularTodayQuestionRetriever>>retrieveQuestions: aUser
	| temp popularTCol averageVotes |
	
	popularTCol := OrderedCollection new.
	cuoora questions do:[:q | (q timestamp asDate = Date today) ifTrue: [popularTCol add: q]].
	averageVotes := (cuoora questions sum: [:q | q positiveVotes size ]) / popularTCol size.
	temp := (popularTCol select:[:q | q positiveVotes size >= averageVotes ]) 
			asSortedCollection:[ :a :b | a positiveVotes size > b positiveVotes size ].
	^ self retrieveQuestionsFor: aUser from: temp. 
</pre>

<pre>
SocialQuestionRetriever>>retrieveQuestions: aUser
	| temp followingCol |
	
	followingCol := OrderedCollection new.
	aUser following do:[ :follow | followingCol addAll: follow questions ].
	temp := followingCol asSortedCollection:[ :a :b | a positiveVotes size > b positiveVotes size ].
	^ self retrieveQuestionsFor: aUser from: temp. 
</pre>

<pre>
TopicsQuestionRetriever>>retrieveQuestions: aUser
	| temp topicsCol |
	
	topicsCol := OrderedCollection new.
	aUser topics do:[ :topic | topicsCol addAll: topic questions ].
	temp := topicsCol asSortedCollection:[ :a :b | a positiveVotes size > b positiveVotes size ].
	^ self retrieveQuestionsFor: aUser from: temp. 
</pre>
>La variable temporal *qRet* ya no es necesaria y se puede quitar de las 4 subclases de ***QuestionRetriever***.

En segundo lugar se extrae el código repetido para el paso 2 en un nuevo método, quedando conformado de la siguiente manera:

<pre>
sortQuestionsByVotes: aCollection
	^ aCollection asSortedCollection: [ :a :b | a positiveVotes size > b positiveVotes size ].
</pre>

Al igual que en el caso anterior, esta funcionalidad es idéntica para todas las subclases, y por lo tanto realizamos un **Pull Up Method**.

<pre>
QuestionRetriever>>sortQuestionsByVotes: aCollection
	^ aCollection asSortedCollection: [ :a :b | a positiveVotes size > b positiveVotes size ].
</pre>

Luego de aplicar este refactoring, el método *retrieveQuestions: aUser* quedaría implementado de esta manera en cada una de las subclases:

<pre>
NewsQuestionRetriever>>retrieveQuestions: aUser
	| temp newsCol |
	
	newsCol := OrderedCollection new.
	cuoora questions do:[:q | (q timestamp asDate = Date today) ifTrue: [newsCol add: q]].
	temp := self sortQuestionsByVotes: newsCol.
	^ self retrieveQuestionsFor: aUser from: temp. 
</pre>

<pre>
PopularTodayQuestionRetriever>>retrieveQuestions: aUser
	| temp popularTCol averageVotes |
	
	popularTCol := OrderedCollection new.
	cuoora questions do:[:q | (q timestamp asDate = Date today) ifTrue: [popularTCol add: q]].
	averageVotes := (cuoora questions sum: [:q | q positiveVotes size ]) / popularTCol size.
	temp := self sortQuestionsByVotes: (popularTCol select:[:q | q positiveVotes size >= averageVotes ]).
	^ self retrieveQuestionsFor: aUser from: temp. 
</pre>

<pre>
SocialQuestionRetriever>>retrieveQuestions: aUser
	| temp followingCol |
	
	followingCol := OrderedCollection new.
	aUser following do: [ :follow | followingCol addAll: follow questions ].
	temp := self sortQuestionsByVotes: followingCol.
	^ self retrieveQuestionsFor: aUser from: temp.
</pre>

<pre>
TopicsQuestionRetriever>>retrieveQuestions: aUser
	| temp topicsCol |
	
	topicsCol := OrderedCollection new.
	aUser topics do: [ :topic | topicsCol addAll: topic questions ].
	temp := self sortQuestionsByVotes: topicsCol.
	^ self retrieveQuestionsFor: aUser from: temp
</pre>

Por último se realiza el **Extract Method** para el paso 1, pero en este caso no se realiza el **Pull Up Method** ya que cada subclase implementa este paso de una forma particular. A continuación se muestran los snippet del código resultante para cada subclase:

<pre>
NewsQuestionRetriever>>getQuestionsFrom: aCollection
	| newsCol |
	
	newsCol := OrderedCollection new.
	aCollection do:[:q | (q timestamp asDate = Date today) ifTrue: [newsCol add: q]].
	^ newsCol
</pre>

<pre>
NewsQuestionRetriever>>retrieveQuestions: aUser
	| temp newsCol |

	newsCol := self getQuestionsFrom: cuoora questions.
	temp := self sortQuestionsByVotes: newsCol.
	^ self retrieveQuestionsFor: aUser from: temp. 
</pre>

<pre>
PopularTodayQuestionRetriever>>getQuestionsFrom: aCollection
	| popularTCol |
	
	popularTCol := OrderedCollection new.
	aCollection do:[:q | (q timestamp asDate = Date today) ifTrue: [popularTCol add: q]].
	^ popularTCol
</pre>

<pre>
PopularTodayQuestionRetriever>>retrieveQuestions: aUser
	| temp popularTCol averageVotes |
	
	popularTCol := self getQuestionsFrom: cuoora questions. 
	
	averageVotes := (cuoora questions sum: [:q | q positiveVotes size ]) / popularTCol size.
	temp := self sortQuestionsByVotes: (popularTCol select:[:q | q positiveVotes size >= averageVotes ]).
	^ self retrieveQuestionsFor: aUser from: temp. 
</pre>

<pre>
SocialQuestionRetriever>>getQuestionsFrom: aCollection
	| followingCol |
	
	followingCol := OrderedCollection new.
	aCollection do: [ :follow | followingCol addAll: follow questions ].
	^ followingCol
</pre>

<pre>
SocialQuestionRetriever>>retrieveQuestions: aUser
	| temp followingCol |
	
	followingCol := self getQuestionsFrom: aUser following.
	temp := self sortQuestionsByVotes: followingCol.
	^ self retrieveQuestionsFor: aUser from: temp.
</pre>

<pre>
TopicsQuestionRetriever>>getQuestionsFrom: aCollection
	| topicsCol |
	
	topicsCol := OrderedCollection new.
	aCollection do: [ :topic | topicsCol addAll: topic questions ].
	^ topicsCol
</pre>

<pre>
TopicsQuestionRetriever>>retrieveQuestions: aUser
	| temp topicsCol |
	
	topicsCol := self getQuestionsFrom: aUser topics.
	temp := self sortQuestionsByVotes: topicsCol.
	^ self retrieveQuestionsFor: aUser from: temp
</pre>
____________________________________________________________________

<p><em>Bad smell</em>: Romper encapsulamiento. </p>
<p>Los valores de las variables de instancia deberian ser seteadas solo cuando son creadas, y no deberían cambiar luego.
En este caso se utilizan los métodos setters en ambos constructores, para inicializar el objeto. Estos setters luego pueden generear que se rompa el encapsulamiento, modificando por fuera de la propia clase Vote sus atributos. </p>
<strong>Vote(class)>>user: aUser likesPublication: aPublication</strong>
<pre>
  ^ self new
  user: aUser;
  publication: aPublication;
  yourself
</pre>

<strong>Vote(class)>>user: aUser dislikesPublication: aPublication</strong>
<pre>
  ^ self new
  user: aUser;
  publication: aPublication;
  dislike;
  yourself
</pre>

<strong>Vote>>user: anObject</strong>
<pre>
  user: anObject
</pre>

<strong>Vote>>publication: anObject</strong>
<pre>
  publication: anObject
</pre>

<p><em>Refactoring</em>: Remove setting Method. </p>
<p>Eliminar los setters (verificando con antelación que no se utilizan) e inicializar los valores de las variables de instancia directamente mediante el constructor, agregando un método con protocolo privado que reciba parámetros para setear los atributos que sean necesarios.</p>

<strong>Vote(private)>>initWithUser: anUser publication: aPublication</strong>
<pre>
  user:= anUser.
  publication: aPublication
</pre>

<strong>Vote(class)>>user: aUser likesPublication: aPublication</strong>
<pre>
  ^ self new
  initWithUser: aUser publication: aPublication;
  yourself
</pre>

<strong>Vote(class)>>user: aUser dislikesPublication: aPublication</strong>
<pre>
  ^ self new
  initWithUser: aUser publication: aPublication;
  dislike;
  yourself
</pre>

<p><strong>Nota:</strong> Este bad smell se encuentra también en las clases Answer, Question, Topic, y User. Decidimos aplicar el mismo refactoring a dichas clases, procediendo de de la misma forma (eliminar <em>setters</em> innecesarios, agregar un método de instancia privado que inicie las variables de instancia y por supuesto modificar el constructor) </p>

<hr>
<strong>Question(class)>>newWithTitle: title description: aDescription user: aUser</strong>
<pre>
  ^ self new
	title: title;
	description: aDescription;
	user: aUser;
	yourself.
</pre>

<strong>Question(class)>>newWithTitle: title description: aDescription user: aUser topic: aTopic</strong>
<pre>
  ^ self new
  	title: title;
	description: aDescription;
	user: aUser;
	addTopic: aTopic;
  	yourself.
</pre>

<p><em>Bad smell</em>: Codigo duplicado. </p>
<p>Al momento de realizar el refactoring anterior () sobre la clase Question, notamos que utiliza dos constructores muy similares, que tienen código duplicado.</p>
<p><em>Refactoring</em>: Remove setting Method. </p>
<p>Además de eliminar los setters de la forma antes mencionada, decidimos eliminar el constructor <strong>Question(class)>>newWithTitle: title description: aDescription user: aUser</strong>, por el hecho de que el otro constructor es más completo y más utilizado. En consecuencia, modificamos el setUp de QuestionTest para que utilice el nuevo constructor.</p>

<strong>Question(class)>>newWithTitle: aTitle description: aDescription user: aUser topic: aTopic</strong>
<pre>
  ^ self new
  initWithTitle: aTitle description: aDescription user: aUser topic: aTopic;
  yourself
</pre>

<strong>Question>>initWithTitle: aTitle description: aDescription user: aUser topic: aTopic</strong>
<pre>
  title := aTitle.
  description := aDescription.
  user := aUser.
  self addTopic: aTopic.
</pre>

<p>Snippet del código antes de la eliminación del constructor</p>
<strong>QuestionTest>>setUp</strong>
<pre>
    question := Question newWithTitle: 'Question  title' description: 'Question description' user: (User new) 
</pre>

<p>Luego del refactoring aplicado:</p>
<strong>QuestionTest>>setUp</strong>
<pre>
    question := Question newWithTitle: 'Question  title' description: 'Question description' user: (User new) topic: (Topic new)
</pre>

<hr>
 

