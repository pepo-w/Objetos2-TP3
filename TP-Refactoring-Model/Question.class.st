Class {
	#name : #Question,
	#superclass : #Publication,
	#instVars : [
		'title',
		'answers',
		'topics'
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

{ #category : #accessing }
Question >> title [
	^title 
]

{ #category : #accessing }
Question >> topics [
	^topics 
]
