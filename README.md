# Objetos2-TP3 
## REGISTRO DE BAD SMELLS Y REFACTORING
#### *Bad smell*: Duplicated Code 

Encontramos que las clases **Answer** y **Question** tienen atributos en común (variables de instancia: *timestamp, description, user* y *votes*) y también ambas clases responden a los mensajes *#addVote:, #positiveVotes, #negativeVotes*, con idéntica funcionalidad.

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

>**Nota**: Además, alguien me puede explicar porque se llama  **| r |** la variable?  

<pre>
Question>>addVote: aVote
	votes add: aVote
</pre>

<pre>
Answer>>addVote: aVote
	votes add: aVote
</pre>

El código duplicado también se encuentra en los métodos *>>initialize* de las clases **Question** y **Answer**:

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

Para resolver el *Bad smell* se aplicaron los siguientes *Refactorings*:

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

También se promocionaron a la superclase los *getters* y *setters* que tenían en común dichas clases.

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
>**Question** realiza un *LookUp* hacia la superclase para instanciar *votes* y *timestamp*, y luego inicializa sus atributos particulares. Por su parte, **Answer** ya no necesita su propio *>>initialize*.
________________________________________________________

#### *Bad smell*: Reinventando la rueda

Se itera sobre la colección *votes* con '*do:*', cuando puede obtenerse el mismo resultado de forma más legible con otros mensajes ya implementados para colecciones.

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

*Refactoring*: **Substitute Algorithm**.

Se utiliza *select:* en lugar de *do*, eliminando la expresión *ifTrue:*, y se retorna el resultado de la operación. Se elimina también la variable local. 

<pre>
Publication>>positiveVotes
	^ votes select: [ :vote | vote isLike ].
</pre>

En el caso de *#negativeVotes* la solución es muy similar, la única diferencia es que se utiliza el mensaje *reject:*:

<pre>
Publication>>negativeVotes
	^ votes reject: [ :vote | vote isLike ].
</pre>

Con este *refactoring* se gana legibilidad y se expresa la intención del código de manera más clara.

____________________________________________________________________

#### *Bad smell*: Switch Statements

Analizando el método *#retrieveQuestions: aUser* de la clase **QuestionRetriever**, observamos que el código consiste principalmente en una secuencia de "if option = #symbol", y en cada caso realiza una serie de operaciones para retornar una colección de instancias de **Question** (según distintos criterios). Esto se considera una mala práctica en OO, ya que puede obtenerse el mismo funcionamiento aplicando polimorfismo. 

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
			temp := (popularTCol select:[:q | q positiveVotes size >= averageVotes ]) 
						asSortedCollection:[ :a :b | a positiveVotes size > b positiveVotes size ].
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

>**QuestionRetriever** ahora es una clase Abstracta

2. *Refactoring*: **Replace Conditional With Polymorphism**.

A continuación utilizamos **Extract Method** para el código asociado a cada uno de los símbolos *#social, #topics, #news, #popularToday*, y los implementamos en las respectivas subclases usando **Move Method**, bajo la firma *#RetrieveQuestions: aUser*. 
Para cada subclase se utilizarán sólo las variables temporales que necesite, y se incluye el retorno del método: <code>^qRet reject:[:q | q user = aUser].</code> , dado que es común para todas las subclases.

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

Una vez que el método se encuentra redefinido por todas las subclases, modificamos el método en la superclase para que el mismo sea abstracto. 
<pre>
QuestionRetriever(Abstract)>>retrieveQuestions: aUser
	^ self subclassResponsibility.
</pre>

Es claro que ya no es necesario utilizar símbolos y condicionales, gracias al uso de polimorfismo, y por lo tanto la variable de instancia *-option* que ahora heredan las subclases ya no se utiliza. Tanto la variable como su setter *#option: anOption*, tienen olor a **Dead Code** (por el hecho de que ya no se usan), y en consecuencia *#initialize* y uno de los constructores de clase dejan de tener sentido.

<pre>
QuestionRetriever>>initialize
	option := #social
</pre>

<pre>
QuestionRetriever(Class side)>>new: cuoora and: aSymbol
	^ self new cuoora: cuoora; option:aSymbol; yourself.
</pre>

Para este refactoring simplemente eliminamos la variable *-option* y los métodos mencionados. 
Al ejecutar los tests correspondientes luego del refactoring, encontramos que los test fallaron dado que el constructor eliminado era utilizado en el setUp de **QuestionRetrieverTest**:

<pre>
QuestionRetrieverTest>>setUp
	(...)
  
   socialRetriever := QuestionRetriever new: cuoora and: #social.
   topicsRetriever := QuestionRetriever new: cuoora and: #topics.
   newsRetriever := QuestionRetriever new: cuoora and: #news.
   popularTodayRetriever := QuestionRetriever new: cuoora and: #popularToday.
</pre>

Dado que ahora **QuestionRetriever** es una clase abstracta, sería incorrecto instanciarla. 
Para solucionar esto sin alterar la funcionalidad de los test, reescribimos el código para que se instancie, en cada caso, un objeto de las subclases de QuestionRetriever. Se utiliza el constructor de la superclase que recibe como parámetro una instancia de cuoora.

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

En las subclases de **QuestionRetriever** se puede observar que hay código duplicado en el método *#retrieveQuestions: aUser*, con algunas particularidades que los diferencian. Identificamos que, de forma general, el algoritmo sigue los mismos pasos, y en el mismo orden:
1. Obtener una colección de instancias de **Question**, relevantes para cada subclase (ya sea desde el parámetro *aUser* o la variable de instancia *-cuoora*).
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

Para la aplicación de este refactoring, en primer lugar hacemos **Extract Method** en todas las subclases para cada uno de los pasos mencionados, con un nombre en común que exprese la intención del paso. Para obtener mayor legibilidad y mantener el mismo nombre de método para todas las subclases, aplicamos el refactoring **Parameterize Method**.

Primero se extrae el código repetido para el paso 3 en un nuevo método *retrieveQuestionsFor: aUser from: aCollection*, quedando conformado de la siguiente manera:

<pre>
retrieveQuestionsFor: aUser from: aCollection
	| qRet |
	qRet := aCollection last: (100 min: aCollection size).
	^qRet reject:[:q | q user = aUser].
</pre>

Se observa que esta porción de código se encuentra en forma idéntica en todas las subclases, por lo que la implementación del método puede estar definida en ***QuestionRetriever***, realizando **Pull Up Method**.

<pre>
QuestionRetriever>>retrieveQuestionsFor: aUser from: aCollection
	| qRet |
	qRet := aCollection last: (100 min: aCollection size).
	^qRet reject:[:q | q user = aUser].
</pre>

Finalmente, en el método *retrieveQuestions: aUser* de las subclases se utiliza el mensaje implementado en la superclase con *self*.

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
>La variable temporal *qRet* se vuelve innecesaria y se quita de las 4 subclases.

En segundo lugar se extrae el código repetido para el paso 2 en un nuevo método conformado de la siguiente manera:

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

Por último se realiza el **Extract Method** para el paso 1, pero en este caso no se realiza el **Pull Up Method** ya que cada subclase implementa este paso de una forma particular. En cambio, se implementa *getQuestionsFor: aUser* como un método abstracto en la superclase **QuestionRetriever**. A continuación se muestra el código resultante para cada clase:

<pre>
NewsQuestionRetriever>>getQuestionsFor: aUser
	| newsCol |
	
	newsCol := OrderedCollection new.
	cuoora questions do:[:q | (q timestamp asDate = Date today) ifTrue: [newsCol add: q]]. 
	^ newsCol
</pre>

<pre>
PopularTodayQuestionRetriever>>getQuestionsFor: aUser
	| popularTCol averageVotes |
	
	popularTCol := OrderedCollection new.
	cuoora questions do:[:q | (q timestamp asDate = Date today) ifTrue: [popularTCol add: q]].
	averageVotes := (cuoora questions sum: [:q | q positiveVotes size ]) / popularTCol size. 
	^ (popularTCol select: [:q | q positiveVotes size >= averageVotes ]) 
</pre>

<pre>
SocialQuestionRetriever>>getQuestionsFor: aUser
	| followingCol |
	
	followingCol := OrderedCollection new.
	aUser following do: [ :follow | followingCol addAll: follow questions ].
	^ followingCol
</pre>

<pre>
TopicsQuestionRetriever>>getQuestionsFor: aUser
	| topicsCol |
	
	topicsCol := OrderedCollection new.
	aUser topics do: [ :topic | topicsCol addAll: topic questions ].
	^ topicsCol
</pre>

<pre>
QuestionRetriever(Abstract)>>getQuestionsFor: aUser
	^ self subclassResponsibility.
</pre>

>**Nota**: Se puede observar que en las clases **NewsQuestionRetriever** y **PopularTodayQuestionRetriever** se recibe un parámetro *aUser* pero no se utiliza en el método. En general, para estos casos se utiliza el refactoring **Remove Parameter**, pero este es un caso particular en el que nos interesa crear un metodo template que sea implementado de la misma forma por todas las subclases, y por lo tanto los nombres de los métodos involucrados deben ser iguales para todas (incluyendo los parametros).

Luego de realizar estos refactoring, llegamos a que el método *RetrieveQuestions: aUser* sea idéntico en todas las subclases (la única diferencia es el nombre de una variable local). Procedemos a hacer un **Pull Up Method**, y para resolver el problema del nombre de la variable temporal aplicamos **Replace Temp With Query** de forma que no sea necesario utilizarla. Además cambiamos el nombre de la variable | temp | por uno un poco más expresivo, y en los métodos que reciben esa variable como parámetro también cambiamos el nombre para que queden iguales, ganando legibilidad. Luego se elimina el método de las subclases para que utilicen el de la superclase.

<pre>
QuestionRetriever>>retrieveQuestions: aUser
	| questions |

	questions := self sortQuestionsByVotes: (self getQuestionsFor: aUser).
	^ self retrieveQuestionsFor: aUser from: questions. 
</pre>

>**Nota**: también se podría remover la variable | questions | utilizando **Replace Temp With Query**, pero creemos que esto le quitaría legibilidad al código ya que quedarían los tres pasos condensados en una sola línea. 

De esta manera, queda formado un método template que generaliza los pasos del algoritmo, e implementa el comportamiento que comparten las subclases de **QuestionRetriever**. A su vez cada subclase redefine el paso 1 de forma particular.
____________________________________________________________________

#### *Bad smell*: Feature Envy

Observamos que **QuestionRetriever** y sus subclases tienen varios métodos que presentan este bad smell. En pocas palabras, en estos métodos se realizan operaciones con atributos y métodos que pertenecen a las clases **User**, **CuOOra** 

____________________________________________________________________

Se me ocurre que podemos usar **Encapsulate Collection** en las clases que tienen getters para colecciones. Para el caso de Smalltalk sería retornar una copia.
#answers --> ^ answers copy. en vez de ^ answers.
en algunos casos tambien se podria usar **Self Encapsulate Field**. por ejemplo
User>>questionsOfInterest
	^ questionRetriever retrieveQuestions: self.
podria ser
User>>questionsOfInterest
	^ self questionRetriever retrieveQuestions: self.
(ya tiene implementado el getter)

