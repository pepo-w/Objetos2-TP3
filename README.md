# Objetos2-TP3
refactoring de cuoora implementado por catedra.

<h2>REGISTRO DE BAD SMELLS Y REFACTORING</h2>

<strong>Answer>>positiveVotes</strong><br>
<code>  | r | </code><br>
<code>  r := OrderedCollection new. </code><br>
<code>  votes do:[:vote | vote isLike ifTrue:[r add: vote]]. </code><br>
<code>  ^r </code>

<p>Bad smell: reinventando la rueda o duplicated code.
  Se itera sobre la coleccion votes con 'do', cuando puede obtenerse el mismo resultado con otro metodo de la clase OrderedCollection. </p>
<p>Refactoring: replace algorithm.
  Se utiliza 'select' en lugar de 'do' y se retorna el resultado de la operación. Se elimina la variable local.
</p>
<code>  ^ votes select: [ :vote | vote isLike ]. </code>
<br><br>

<strong>Answer>>negativeVotes</strong><br>
<code>  | r |  </code><br>
<code>  r := OrderedCollection new.  </code><br>
<code>  votes do:[:vote | vote isLike ifFalse:[r add: vote]].  </code><br>
<code>  ^r  </code>

<p>Bad smell: reinventando la rueda o duplicated code.
  Se itera sobre la coleccion votes con 'do', cuando puede obtenerse el mismo resultado con otro metodo de la clase OrderedCollection. 
</p>
<p>Refactoring: replace algorithm.
  Se utiliza 'reject' en lugar de 'do', eliminando la evaluacion 'ifFalse:', y se retorna el resultado de la operación. Se elimina la variable local.
</p>
<code>
  ^ votes reject: [ :vote | vote isLike ].
</code>
<br><br>
