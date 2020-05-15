Class {
	#name : #QuestionRetriever,
	#superclass : #Object,
	#instVars : [
		'cuoora'
	],
	#category : #'TP-Refactoring-Model'
}

{ #category : #'instance creation' }
QuestionRetriever class >> new: cuoora [
	^ self new cuoora: cuoora
]

{ #category : #accesing }
QuestionRetriever >> cuoora: aCuooraInstance [
	cuoora := aCuooraInstance 
]

{ #category : #retrieving }
QuestionRetriever >> getQuestions: aUser [
	^ self subclassResponsibility.
]

{ #category : #retrieving }
QuestionRetriever >> retrieveQuestions: aUser [
	| temp aCollection |

	aCollection := self getQuestions: aUser. 
	temp := self sortQuestionsByVotes: aCollection.
	^ self retrieveQuestionsFor: aUser from: temp. 
]

{ #category : #'as yet unclassified' }
QuestionRetriever >> retrieveQuestionsFor: aUser from: aCollection [
	| qRet |
	
	qRet := aCollection last: (100 min: aCollection size).
	^qRet reject:[:q | q user = aUser].
]

{ #category : #sorting }
QuestionRetriever >> sortQuestionsByVotes: aCollection [
	^ aCollection asSortedCollection: [ :a :b | a positiveVotes size > b positiveVotes size ].
]
