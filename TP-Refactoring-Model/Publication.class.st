Class {
	#name : #Publication,
	#superclass : #Object,
	#instVars : [
		'timestamp',
		'description',
		'user',
		'votes'
	],
	#category : #'TP-Refactoring-Model'
}

{ #category : #adding }
Publication >> addVote: aVote [
	votes add: aVote
]

{ #category : #accessing }
Publication >> description [
	^ description
]

{ #category : #'as yet unclassified' }
Publication >> negativeVotes [
	| r |
	r := OrderedCollection new.
	votes do:[:vote | vote isLike ifFalse:[r add: vote]].
	^r
]

{ #category : #'as yet unclassified' }
Publication >> positiveVotes [
	| r |
	r := OrderedCollection new.
	votes do:[:vote | vote isLike ifTrue:[r add: vote]].
	^r
]

{ #category : #accessing }
Publication >> timestamp [
	^ timestamp
]

{ #category : #accessing }
Publication >> user [
	^ user
]

{ #category : #accessing }
Publication >> votes [
	^ votes
]
