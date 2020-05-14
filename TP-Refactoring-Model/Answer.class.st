Class {
	#name : #Answer,
	#superclass : #Publication,
	#instVars : [
		'question'
	],
	#category : #'TP-Refactoring-Model'
}

{ #category : #'instance creation' }
Answer class >> new: aDescription user: aUser question: aQuestion [
	^ self new
		initWithDescription: aDescription user: aUser question: aQuestion;
		yourself
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
