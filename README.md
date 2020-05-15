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

<strong>Publication>>positiveVotes</strong>
<pre>
  | r | 
  r := OrderedCollection new. 
  votes do:[:vote | vote isLike ifTrue:[r add: vote]]. 
  ^r
</pre>

<p><em>Bad smell</em>: reinventando la rueda o duplicated code.</p>
<p>Se itera sobre la coleccion votes con 'do', cuando puede obtenerse el mismo resultado con otro metodo ya implementado para colecciones (select). </p>
<p><em>Refactoring</em>: substitute algorithm.</p>
<p>Se utiliza 'select' en lugar de 'do', eliminando la expresión 'ifTrue', y se retorna el resultado de la operación. Se elimina la variable local. </p>
<pre>  ^ votes select: [ :vote | vote isLike ]. </pre>
<hr>

<strong>Publication>>negativeVotes</strong>
<pre>
  | r |
  r := OrderedCollection new.  
  votes do:[:vote | vote isLike ifFalse:[r add: vote]]. 
  ^r
</pre>

<p><em>Bad smell</em>: reinventando la rueda o duplicated code.</p>
<p>Se itera sobre la coleccion votes con 'do', cuando puede obtenerse el mismo resultado con otro metodo ya implementado para colecciones (reject). </p>
<p><em>Refactoring</em>: substitute algorithm.</p>
<p>Se utiliza 'reject' en lugar de 'do', eliminando la expresión 'ifFalse:', y se retorna el resultado de la operación. Se elimina la variable local. </p>
<pre>  ^ votes reject: [ :vote | vote isLike ]. </pre>
<hr>
 

