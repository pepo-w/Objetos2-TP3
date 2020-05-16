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
QuestionRetriever >> getQuestionsFor: aUser [
	^ self subclassResponsibility.
]

{ #category : #retrieving }
QuestionRetriever >> retrieveQuestions: aUser [
	| questions |

	questions := self sortQuestionsByVotes: (self getQuestionsFor: aUser).
	^ self retrieveQuestionsFor: aUser from: questions. 
]

{ #category : #'as yet unclassified' }
QuestionRetriever >> retrieveQuestionsFor: aUser from: questions [
	| qRet |
	
	qRet := questions last: (100 min: questions size).
	^qRet reject:[:q | q user = aUser].
]

{ #category : #sorting }
QuestionRetriever >> sortQuestionsByVotes: questions [
	^ questions asSortedCollection: [ :a :b | a positiveVotes size > b positiveVotes size ].
]
