Class {
	#name : #CuOOra,
	#superclass : #Object,
	#instVars : [
		'users',
		'topics',
		'questions'
	],
	#category : #'TP-Refactoring-Model'
}

{ #category : #adding }
CuOOra >> addQuestion: aQuestion [
	questions add: aQuestion
]

{ #category : #adding }
CuOOra >> addQuestion: aQuestion forUser: aUser [
	aUser addQuestion: aQuestion.
	questions add: aQuestion.
]

{ #category : #adding }
CuOOra >> addTopic: aTopic [
	topics add: aTopic 

]

{ #category : #adding }
CuOOra >> addUser: aUser [
	users add: aUser 

]

{ #category : #private }
CuOOra >> averageVotes [
	^ (self questions sumNumbers: [:q | q sizeOfPositiveVotes ]) / self todayQuestions size.
]

{ #category : #initialize }
CuOOra >> initialize [
	users := OrderedCollection new.
	topics := OrderedCollection new.
	questions := OrderedCollection new
]

{ #category : #accessing }
CuOOra >> questions [
	^ questions
]

{ #category : #'collection getters' }
CuOOra >> todayQuestions [
	^ questions select: [ :q | q isFromToday ].
]

{ #category : #'collection getters' }
CuOOra >> todayQuestionsAboveAverage [
	^ self todayQuestions select: [ :q | q sizeOfPositiveVotes >= self averageVotes ].
]

{ #category : #accessing }
CuOOra >> topics [
	^ topics
]

{ #category : #accessing }
CuOOra >> users [
	^ users
]
