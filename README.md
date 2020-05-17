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

>***QuestionRetriever*** ahora es una clase Abstracta

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
QuestionRetriever>>retrieveQuestions: aUser
	^ self subclassResponsibility.
</pre>

En este punto vemos que ya no es necesario utilizar símbolos y condicionales, gracias al uso de polimorfismo, y por lo tanto la variable de instancia *option* que ahora heredan las subclases ya no se utiliza. Tanto la variable como su setter *#option: anOption*, tienen olor a **Dead Code** (por el hecho de que ya no se usan), y en consecuencia *#initialize* y uno de los constructores de clase dejan de tener sentido.

<pre>
QuestionRetriever>>initialize
	option := #social
</pre>

<pre>
QuestionRetriever(Class side)>>new: cuoora and: aSymbol
	^ self new cuoora: cuoora; option:aSymbol; yourself.
</pre>

Para este refactoring simplemente eliminamos la variable *option* y los métodos mencionados. 
Al ejecutar los tests correspondientes luego del refactoring, encontramos que los test fallaron dado que el constructor eliminado era utilizado en el setUp de **QuestionRetrieverTest**:

<pre>
QuestionRetrieverTest>>setUp
	(...)
  
   socialRetriever := QuestionRetriever new: cuoora and: #social.
   topicsRetriever := QuestionRetriever new: cuoora and: #topics.
   newsRetriever := QuestionRetriever new: cuoora and: #news.
   popularTodayRetriever := QuestionRetriever new: cuoora and: #popularToday.
</pre>

Dado que ahora ***QuestionRetriever*** es una clase abstracta, sería incorrecto instanciarla. 
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

En las subclases de ***QuestionRetriever*** se puede observar que hay código duplicado en el método *#retrieveQuestions: aUser*, con algunas particularidades que los diferencian. Identificamos que, de forma general, el algoritmo sigue los mismos pasos, y en el mismo orden:
1. Obtener una colección de instancias de **Question**, relevantes para cada subclase (ya sea desde el parámetro *aUser* o la variable de instancia *cuoora*).
2. Ordenar dicha colección en forma ascendente según la cantidad de votos positivos de cada question.
3. Filtrar la colección para retornar las últimas 100 questions (o todas si son menos de 100) que no fueron realizadas por *aUser*.

<pre>
NewsQuestionRetriever>>retrieveQuestions: aUser
	| qRet temp newsCol |
	
	"paso 1"
	qRet := OrderedCollection new.	
	newsCol := OrderedCollection new.
	cuoora questions do:[:q | (q timestamp asDate = Date today) ifTrue: [newsCol add: q]].
	
	"paso 2"
	temp := newsCol asSortedCollection:[ :a :b | a positiveVotes size > b positiveVotes size ].
	
	"paso 3"
	qRet := temp last: (100 min: temp size).
	^qRet reject:[:q | q user = aUser].
</pre>

<pre>
PopularTodayQuestionRetriever>>retrieveQuestions: aUser
	| qRet temp popularTCol averageVotes |
	
	"paso 1"
	popularTCol := OrderedCollection new.
	cuoora questions do:[:q | (q timestamp asDate = Date today) ifTrue: [popularTCol add: q]].
	averageVotes := (cuoora questions sum: [:q | q positiveVotes size ]) / popularTCol size.
	
	"paso 2 (y un poco de paso 1)"
	temp := (popularTCol select:[:q | q positiveVotes size >= averageVotes ]) asSortedCollection:[ :a :b | a positiveVotes size > b positiveVotes size ].
	
	"paso 3"
	qRet := temp last: (100 min: temp size).
	^qRet reject:[:q | q user = aUser].
</pre>

<pre>

SocialQuestionRetriever>>retrieveQuestions: aUser
	| qRet temp followingCol |
	
	"paso 1"
	followingCol := OrderedCollection new.
	aUser following do:[ :follow | followingCol addAll: follow questions ].
	
	"paso 2"
	temp := followingCol asSortedCollection:[ :a :b | a positiveVotes size > b positiveVotes size ].
	
	"paso 3"
	qRet := temp last: (100 min: temp size).
	^qRet reject:[:q | q user = aUser].	
</pre>

<pre>
TopicsQuestionRetriever>>retrieveQuestions: aUser
retrieveQuestions: aUser
	| qRet temp topicsCol |
	
	"paso 1"
	topicsCol := OrderedCollection new.
	aUser topics do:[ :topic | topicsCol addAll: topic questions ].
	
	"paso 2"
	temp := topicsCol asSortedCollection:[ :a :b | a positiveVotes size > b positiveVotes size ].
	
	"paso 3"
	qRet := temp last: (100 min: temp size).
	^qRet reject:[:q | q user = aUser].
</pre>
>**Nota**: se agregan comentarios sólo de manera momentánea para organizar un poco el refactoring.

*Refactoring*: **Form Template Method**

Para la aplicación de este refactoring, en primer lugar hacemos **Extract Method** en todas las subclases para cada uno de los pasos mencionados, con un nombre en común que exprese la intención del paso. Los métodos creados se hacen con protocolo *private*, ya que consisten en funcionamiento interno para realizar *#retrieveQuestions: aUser*, de modo que este último sea el único método visible para **User**. Para obtener mayor legibilidad y mantener el mismo nombre de método para todas las subclases, aplicamos el refactoring **Parameterize Method**.

Primero aplicamos **Extract Method** con el código repetido para el **paso 3** en un nuevo método *#retrieveQuestionsFor: aUser from: questions*. Se observa que esta porción de código se encuentra en forma idéntica en todas las subclases, por lo tanto el método puede estar definido en la superclase ***QuestionRetriever***, realizando **Pull Up Method**.

<pre>
QuestionRetriever>>retrieveQuestionsFor: aUser from: questions
	| qRet |
	qRet := questions last: (100 min: questions size).
	^qRet reject:[:q | q user = aUser].
</pre>

Observando el código es claro que el método hace dos cosas: obtener las últimas 100 preguntas de *questions* y filtrar aquellas cuyo user coincide con *aUser*. Decidimos aplicar **Extract Method** para separar ambos pasos en métodos privados, de manera que el método resulte más expresivo respecto a qué hace. 

<pre>
QuestionRetriever>>lastQuestions: questions 
	^ questions last: (100 min: questions size).
</pre>

<pre>
QuestionRetriever>>rejectQuestionsWithAuthor: aUser from: questions 
	^ questions reject: [ :q | q user = aUser ].
</pre>

Finalmente aplicamos **Replace Temp With Query** para remover la variable local | qRet |. 

<pre>
QuestionRetriever>>retrieveQuestionsFor: aUser from: questions
	^ self rejectQuestionsWithAuthor: aUser from: (self lastQuestions: questions).
</pre>

Una vez implementado el **paso 3** en la superclase (que a su vez consiste en dos "sub-pasos"), se reemplaza el código correspondiente por una invocación en el método *retrieveQuestions: aUser* de las subclases. También se elimina la variable temporal *qRet*.

<pre>
NewsQuestionRetriever>>retrieveQuestions: aUser
	| questions newsCol |

	newsCol := OrderedCollection new.
	cuoora questions do:[:q | (q timestamp asDate = Date today) ifTrue: [newsCol add: q]].
	questions := newsCol asSortedCollection:[ :a :b | a positiveVotes size > b positiveVotes size ].
	^ self retrieveQuestionsFor: aUser from: questions. 
</pre>

<pre>
PopularTodayQuestionRetriever>>retrieveQuestions: aUser
	| questions popularTCol averageVotes |
	
	popularTCol := OrderedCollection new.
	cuoora questions do:[:q | (q timestamp asDate = Date today) ifTrue: [popularTCol add: q]].
	averageVotes := (cuoora questions sum: [:q | q positiveVotes size ]) / popularTCol size.
	questions := (popularTCol select:[:q | q positiveVotes size >= averageVotes ]) 
			asSortedCollection:[ :a :b | a positiveVotes size > b positiveVotes size ].
	^ self retrieveQuestionsFor: aUser from: questions. 
</pre>

<pre>
SocialQuestionRetriever>>retrieveQuestions: aUser
	| questions followingCol |
	
	followingCol := OrderedCollection new.
	aUser following do:[ :follow | followingCol addAll: follow questions ].
	questions := followingCol asSortedCollection:[ :a :b | a positiveVotes size > b positiveVotes size ].
	^ self retrieveQuestionsFor: aUser from: questions. 
</pre>

<pre>
TopicsQuestionRetriever>>retrieveQuestions: aUser
	| questions topicsCol |
	
	topicsCol := OrderedCollection new.
	aUser topics do:[ :topic | topicsCol addAll: topic questions ].
	questions := topicsCol asSortedCollection:[ :a :b | a positiveVotes size > b positiveVotes size ].
	^ self retrieveQuestionsFor: aUser from: questions. 
</pre>

En segundo lugar aplicamos **Extract Method** para el **paso 2** en un nuevo método *#sortQuestionsByVotes: questions*.
Al igual que en el caso anterior, esta funcionalidad es idéntica para todas las subclases, y por lo tanto realizamos un **Pull Up Method**.

<pre>
QuestionRetriever>>sortQuestionsByVotes: questions
	^ questions asSortedCollection: [ :a :b | a positiveVotes size > b positiveVotes size ].
</pre>

Luego de aplicar este refactoring, el método *#retrieveQuestions: aUser* quedaría implementado de esta manera en cada una de las subclases:

<pre>
NewsQuestionRetriever>>retrieveQuestions: aUser
	| questions newsCol |
	
	newsCol := OrderedCollection new.
	cuoora questions do:[:q | (q timestamp asDate = Date today) ifTrue: [newsCol add: q]].
	questions := self sortQuestionsByVotes: newsCol.
	^ self retrieveQuestionsFor: aUser from: questions. 
</pre>

<pre>
PopularTodayQuestionRetriever>>retrieveQuestions: aUser
	| questions popularTCol averageVotes |
	
	popularTCol := OrderedCollection new.
	cuoora questions do:[:q | (q timestamp asDate = Date today) ifTrue: [popularTCol add: q]].
	averageVotes := (cuoora questions sum: [:q | q positiveVotes size ]) / popularTCol size.
	questions := self sortQuestionsByVotes: (popularTCol select:[:q | q positiveVotes size >= averageVotes ]).
	^ self retrieveQuestionsFor: aUser from: questions. 
</pre>

<pre>
SocialQuestionRetriever>>retrieveQuestions: aUser
	| questions followingCol |
	
	followingCol := OrderedCollection new.
	aUser following do: [ :follow | followingCol addAll: follow questions ].
	questions := self sortQuestionsByVotes: followingCol.
	^ self retrieveQuestionsFor: aUser from: questions.
</pre>

<pre>
TopicsQuestionRetriever>>retrieveQuestions: aUser
	| questions topicsCol |
	
	topicsCol := OrderedCollection new.
	aUser topics do: [ :topic | topicsCol addAll: topic questions ].
	questions := self sortQuestionsByVotes: topicsCol.
	^ self retrieveQuestionsFor: aUser from: questions
</pre>

Por último se realiza el **Extract Method** para el **paso 1** en cada subclase, pero en este caso no se realiza el **Pull Up Method** ya que cada subclase implementa este paso de una forma particular. En cambio, se implementa *getQuestionsFor: aUser* como un método abstracto en la superclase **QuestionRetriever**.

<pre>
QuestionRetriever>>getQuestionsFor: aUser
	^ self subclassResponsibility.
</pre>

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

>**Nota**: En las clases **NewsQuestionRetriever** y **PopularTodayQuestionRetriever** se recibe un parámetro *aUser* pero no se utiliza en el método. En general, para estos casos se utiliza el refactoring **Remove Parameter**, pero este es un caso particular en el que nos interesa crear un metodo template que sea implementado de la misma forma por todas las subclases, y por lo tanto los nombres de los métodos involucrados deben ser iguales para todas (incluyendo los parametros).

Luego de realizar estos refactoring, llegamos a que el método *RetrieveQuestions: aUser* sea casi idéntico en todas las subclases, la única diferencia es el nombre de una variable temporal. Procedemos a hacer un **Pull Up Method**, y para resolver el problema del nombre de la variable temporal aplicamos **Replace Temp With Query** de forma que no sea necesario utilizarla. Luego se elimina el método de las subclases para que utilicen el de la superclase.

<pre>
QuestionRetriever>>retrieveQuestions: aUser
	| questions |

	questions := self sortQuestionsByVotes: (self getQuestionsFor: aUser).
	^ self retrieveQuestionsFor: aUser from: questions. 
</pre>

Notamos que los pasos 1 y 2 se realizan en la misma línea, y luego se realiza el paso 3. Considerando que los pasos 2 y 3 son iguales para todas las subclases, y que el paso 1 es particular a cada una, creemos que sería más conveniente agrupar los pasos 2 y 3. De hecho si analizamos el comportamiento de cada uno encontramos algo en común: ambos "preparan" o "le dan un formato" la colección de questions antes de retornarla.
Utilizando **Move Method** reorganizamos el código de la siguiente manera:

<pre>
QuestionRetriever>>retrieveQuestionsFor: aUser from: questions
	| sortedQuestions |

	sortedQuestions := self sortQuestionsByVotes: questions.
	^ self rejectQuestionsWithAutor: aUser from: (self lastQuestions: sortedQuestions). 
</pre>

<pre>
QuestionRetriever>>retrieveQuestions: aUser
	^ self retrieveQuestionsFor: aUser from: (self getQuestionsFor: aUser).
</pre>

De esta manera queda formado un método template que generaliza los pasos del algoritmo, e implementa el comportamiento que comparten las subclases de **QuestionRetriever** (pasos 2 y 3). A su vez cada subclase redefine el paso 1 de forma particular. 
Esta forma de organizar el código aporta a que el modelo sea *escalable*: si se decidieran agregar nuevas subclases de **QuestionRetriever**, que obtengan questions bajo nuevos "criterios", basta con utilizar el template method redefiniendo en la nueva subclase aquellos pasos que deben hacerse de forma diferente.
____________________________________________________________________

#### *Bad smell*: Feature Envy

Observamos que **QuestionRetriever** y sus subclases tienen varios métodos que presentan este bad smell. En pocas palabras, en estos métodos se realizan operaciones con atributos y mensajes que pertenecen (la mayoría) a las clases **User** y **CuOOra**. Es decir, le "piden" datos que necesitan de estas clases para realizar distintas operaciones. 
Esto se considera un bad smell, ya que la responsabilidad de implementar una funcionalidad debe recaer en las clases que poseen los datos necesarios para llevar a cabo dicha funcionalidad. 

La forma de refactorizar es similar en todos los casos, consiste en determinar quién tiene la responsabilidad de cada operación, para luego delegar la implementación a la clase que corresponda con **Extract Method** y **Move Method**.

A continuación se pueden ver los métodos con *Feature Envy* y sus respectivos *refactorings*.

**QuestionRetriever>>sortQuestionsByVotes: questions**

<pre>
QuestionRetriever>>sortQuestionsByVotes: questions
	^ questions asSortedCollection: [ :a :b | a positiveVotes size > b positiveVotes size ].
</pre>

*Feature Envy*: ***QuestionRetriever*** accede la colección de votos positivos de **Question**, para luego calcular su tamaño.

*Refactoring*: delegar la responsabilidad de hacer este cálculo a la clase **Publication** (ya que tanto **Question** como **Answer** tienen una colección de votos positivos).

<pre>
Publication>>sizeOfPositiveVotes
	^ self positiveVotes size.
</pre>

<pre>
QuestionRetriever>>sortQuestionsByVotes: questions
	^ questions asSortedCollection: [ :a :b | a sizeOfPositiveVotes > b sizeOfPositiveVotes ].
</pre>


**QuestionRetriever>>rejectQuestionsWithAuthor: aUser from: questions**

<pre>
QuestionRetriever>>rejectQuestionsWithAuthor: aUser from: questions 
	^ questions reject: [ :q | q user = aUser ].
</pre>

*Feature Envy*: ***QuestionRetriever*** accede a la variable *user* de **Question** para verificar si es el mismo user recibido como parámetro.

*Refactoring*: se delega a **Publication** la tarea de verificar si *aUser* es el *user* asociado a un objeto *question* o *answer*.

<pre>
Publication>>hasTheAuthor: aUser
	^ self user = aUser.
</pre>

<pre>
QuestionRetriever>>rejectQuestionsWithAuthor: aUser from: questions 
	^ questions reject: [ :q | q hasTheAuthor: aUser ].
</pre>


**NewsQuestionRetriever>>getQuestionsFor: aUser**

<pre>
NewsQuestionRetriever>>getQuestionsFor: aUser
	| newsCol |
	
	newsCol := OrderedCollection new.
	cuoora questions do:[:q | (q timestamp asDate = Date today) ifTrue: [newsCol add: q]]. 
	^ newsCol
</pre>

En este método se observan dos casos de *Feature Envy*: por un lado **NewsQuestionRetriever** accede a la variable *questions* de **CuOOra** para realizar una operación sobre la colección (además se itera con *#do:*, con olor a **Reinventando la Rueda**), y por otro lado se accede a la variable *timestamp* de **Question** para verificar que sea del día actual. 

*Refactoring*: se delega a **Publication** la responsabilidad de verificar si un objeto *answer* o *question* fue instanciado en el día actual. Y se delega a **CuOOra** la tarea de obtener preguntas del día, utilizando *#select:* en lugar de *#do:*. Luego se simplifica el método reemplazando la variable |newsCol| por una llamada al mensaje de *quoora*.

<pre>
Publication>>isFromToday
	^ self timestamp asDate = Date today.
</pre>

<pre>
CuOOra>>todayQuestions
	^ questions select: [ :q | q isFromToday ].
</pre>

<pre>
NewsQuestionRetriever>>getQuestionsFor: aUser
	^ cuoora todayQuestions.
</pre>


**PopularTodayQuestionRetriever>>getQuestionsFor: aUser**

<pre>
PopularTodayQuestionRetriever>>getQuestionsFor: aUser
	| popularTCol averageVotes |
	
	popularTCol := OrderedCollection new.
	cuoora questions do:[:q | (q timestamp asDate = Date today) ifTrue: [popularTCol add: q]]. 
	averageVotes := (cuoora questions sum: [:q | q positiveVotes size ]) / popularTCol size.
	^ (popularTCol select:[:q | q positiveVotes size >= averageVotes ])
	
</pre>

*Feature Envy*: **PopularTodayQuestionRetriever** accede a la variable *questions* de **CuOOra** para seleccionar aquellas que sean del día actual (ya refactorizado con *#todayQuestions*), y luego realiza un cálculo de promedio con dicha colección para finalmente filtrar aquellas preguntas con votos positivos superiores al promedio.

*Refactoring*: Se delega a **CuOOra** la tarea de obtener las preguntas del día que superen el promedio de votos positivos (así como también la tarea de calcular dicho promedio). 

<pre>
CuOOra>>averageVotes
	^ (questions sumNumbers: [:q | q sizeOfPositiveVotes ]) / self todayQuestions size.
</pre>

<pre>
CuOOra>>todayQuestionsAboveAverage
	^ self todayQuestions select: [ :q | q sizeOfPositiveVotes >= self averageVotes ].
</pre>

<pre>
PopularTodayQuestionRetriever>>getQuestionsFor: aUser	
	^ cuoora todayQuestionsAboveAverage.
</pre>

	
**SocialQuestionRetriever>>getQuestionsFor: aUser**

<pre>
SocialQuestionRetriever>>getQuestionsFor: aUser
	| followingCol |
	
	followingCol := OrderedCollection new.
	aUser following do: [ :follow | followingCol addAll: follow questions ]. 
	^ followingCol
</pre>

*Feature Envy*: **SocialQuestionRetriever** accede a la colección *following* de **User** para obtener las questions de cada uno de los users a los que sigue *aUser*.

*Refactoring*: se delega a **User** la función de obtener las preguntas de los usuarios que sigue, utilizando *#flatCollect:* en lugar de *#do:*. Luego se reemplaza la variable |followingCol| por una llamada al mensaje de *aUser*.

<pre>
User>>followingQuestions
	^ self following flatCollect: [ :f | f questions ]. 
</pre>

<pre>
SocialQuestionRetriever>>getQuestionsFor: aUser
	^ aUser followingQuestions. 
</pre>


**TopicsQuestionRetriever>>getQuestionsFor: aUser**

<pre>
TopicsQuestionRetriever>>getQuestionsFor: aUser
	| topicsCol |
	
	topicsCol := OrderedCollection new.
	aUser topics do: [ :topic | topicsCol addAll: topic questions ]. 
	^ topicsCol
</pre>

*Feature Envy*: **TopicsQuestionRetriever** accede a la colección *topics* de **User** para obtener las questions que tiene cada *topic*.

*Refactoring*: se delega a **User** la función de obtener las preguntas de los topicos que tiene, utilizando *#flatCollect:* en lugar de *#do:*. Luego se reemplaza la variable |topicsCol| por una llamada al mensaje de *aUser*.

<pre>
User>>topicsQuestions
	^ self topics flatCollect: [ :t | t questions ].
</pre>

<pre>
User>>getQuestionsFor: aUser
	^ aUser topicsQuestions.
</pre>
