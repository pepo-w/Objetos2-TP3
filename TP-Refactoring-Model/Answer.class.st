Class {
	#name : #Answer,
	#superclass : #Object,
	#instVars : [
		'question',
		'timestamp',
		'user',
		'votes',
		'description'
	],
	#category : #'TP-Refactoring-Model'
}

{ #category : #'instance creation' }
Answer class >> new: aDescription user: aUser question: aQuestion [
	^ self new
		initWithDescription: aDescription user: aUser question: aQuestion;
		yourself
]

{ #category : #adding }
Answer >> addVote: aVote [
	votes add: aVote
]

{ #category : #accessing }
Answer >> description [
	^ description
]

{ #category : #initialization }
Answer >> initWithDescription: aDescription user: aUser question: aQuestion [
	description := aDescription.
	user := aUser.
	question := aQuestion.
]

{ #category : #initialize }
Answer >> initialize [
	votes := OrderedCollection new.
	timestamp := DateAndTime now.
]

{ #category : #'as yet unclassified' }
Answer >> negativeVotes [
	^ votes reject: [ :vote | vote isLike ].
]

{ #category : #'as yet unclassified' }
Answer >> positiveVotes [
	^ votes select: [ :vote | vote isLike ].
]

{ #category : #accessing }
Answer >> timestamp [
	^ timestamp
]

{ #category : #accessing }
Answer >> user [
	^ user
]

{ #category : #accessing }
Answer >> votes [
	^ votes
]
