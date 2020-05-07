# Objetos2-TP3
<h2>REGISTRO DE BAD SMELLS Y REFACTORING</h2>
<hr>
<strong>Answer>>positiveVotes</strong>
<pre>  
  | r | 
  r := OrderedCollection new. 
  votes do:[:vote | vote isLike ifTrue:[r add: vote]]. 
  ^r 
</pre>

<p><em>Bad smell</em>: reinventando la rueda o duplicated code.</p>
<p>Se itera sobre la coleccion votes con 'do', cuando puede obtenerse el mismo resultado con otro metodo de la clase OrderedCollection. </p>
<p><em>Refactoring</em>: replace algorithm.</p>
<p>Se utiliza 'select' en lugar de 'do', eliminando la expresión 'ifTrue', y se retorna el resultado de la operación. Se elimina la variable local. </p>
<code>  ^ votes select: [ :vote | vote isLike ]. </code>
<br>
<hr>

<strong>Answer>>negativeVotes</strong>
<pre>
  | r |
  r := OrderedCollection new.  
  votes do:[:vote | vote isLike ifFalse:[r add: vote]]. 
  ^r  
</pre>

<p><em>Bad smell</em>: reinventando la rueda o duplicated code.</p>
<p>Se itera sobre la coleccion votes con 'do', cuando puede obtenerse el mismo resultado con otro metodo de la clase OrderedCollection. </p>
<p><em>Refactoring</em>: replace algorithm.</p>
<p>Se utiliza 'reject' en lugar de 'do', eliminando la expresión 'ifFalse:', y se retorna el resultado de la operación. Se elimina la variable local. </p>
<code> ^ votes reject: [ :vote | vote isLike ]. </code>
<br>
<hr>
