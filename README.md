# Objetos2-TP3 
<h2>REGISTRO DE BAD SMELLS Y REFACTORING</h2> 

<hr>
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

<p><em>Bad smell</em>: Romper encapsulamiento. </p>
<p>Los valores de las variables de instancia deberian ser seteadas solo cuando son creadas, y no deberían cambiar luego.
En este caso se utilizan los métodos setters en ambos constructores, para inicializar el objeto. Estos setters luego pueden generear que se rompa el encapsulamiento, modificando por fuera de la propia clase Vote sus atributos. </p>
<p><em>Refactoring</em>: Remove setting Method. </p>
<p>Eliminar los setters e inicializar los valores de las variables de instancia directamente mediante el constructor, agregando un método con protocolo privado que reciba parámetros para setear los atributos que sean necesarios.</p>

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

<p><strong>Nota:</strong> Este bad smell se encuentra también en las clases Answer, Question, Topic, y User. Decidimos aplicar el mismo refactoring a dichas clases. Dado que la implementación es igual (a diferencia de los setters particulares que usa cada clase, y los nombres de los métodos), pensamos que no es necesario mostrar los snippet de código.</p>

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
  	user: aUser;
 	publication: aPublication;
 	dislike;
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
<strong>Answer>>positiveVotes</strong>
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

<strong>Answer>>negativeVotes</strong>
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
 

