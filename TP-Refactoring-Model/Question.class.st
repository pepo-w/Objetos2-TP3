Class {
	#name : #Question,
	#superclass : #Object,
	#instVars : [
		'title',
		'answers',
		'topics',
		'timestamp',
		'user',
		'votes',
		'description'
	],
	#category : #'TP-Refactoring-Model'
}

{ #category : #'instance creation' }
Question class >> newWithTitle: aTitle description: aDescription user: aUser topic: aTopic [
	^ self new
		initWithTitle: aTitle description: aDescription user: aUser topic: aTopic;
		yourself.
]

{ #category : #adding }
Question >> addTopic: aTopic [
	topics add: aTopic.
	aTopic addQuestion: self.

]

{ #category : #adding }
Question >> addVote: aVote [
	votes add: aVote
]

{ #category : #accessing }
Question >> description [
	^ description
]

{ #category : #initialization }
Question >> initWithTitle: aTitle description: aDescription user: aUser topic: aTopic [
	title := aTitle.
	description := aDescription.
	user := aUser.
	self addTopic: aTopic.
]

{ #category : #'initalize-release' }
Question >> initialize [
	answers := OrderedCollection new.
	topics := OrderedCollection new.
	votes := OrderedCollection new.
	timestamp := DateAndTime now.
]

{ #category : #'as yet unclassified' }
Question >> negativeVotes [
	| r |
	r := OrderedCollection new.
	votes do:[:vote | vote isLike ifFalse:[r add: vote]].
	^r
]

{ #category : #'as yet unclassified' }
Question >> positiveVotes [
	| r |
	r := OrderedCollection new.
	votes do:[:vote | vote isLike ifTrue:[r add: vote]].
	^r
]

{ #category : #accessing }
Question >> timestamp [
	^ timestamp
]

{ #category : #accessing }
Question >> title [
	^title 
]

{ #category : #accessing }
Question >> topics [
	^topics 
]

{ #category : #accessing }
Question >> user [
	^ user
]

{ #category : #accessing }
Question >> votes [
	^ votes
]
