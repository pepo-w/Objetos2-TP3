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

{ #category : #private }
QuestionRetriever >> getLastQuestionsFrom: questions [ 
	^ questions last: (self numberOfLastQuestions min: questions size).
]

{ #category : #retrieving }
QuestionRetriever >> getQuestionsFor: aUser [
	^ self subclassResponsibility.
]

{ #category : #private }
QuestionRetriever >> numberOfLastQuestions [
	^ 100.
]

{ #category : #private }
QuestionRetriever >> rejectQuestionsFrom: questions withAuthor: aUser [
	^ questions reject: [ :q | q hasTheAuthor: aUser ].
]

{ #category : #retrieving }
QuestionRetriever >> retrieveQuestions: aUser [
	| questions |

	questions := self sortQuestionsByVotes: (self getQuestionsFor: aUser).
	^ self retrieveQuestionsFor: aUser from: questions. 
]

{ #category : #private }
QuestionRetriever >> retrieveQuestionsFor: aUser from: questions [
	
	^ self rejectQuestionsFrom: (self getLastQuestionsFrom: questions) withAuthor: aUser.
	
]

{ #category : #private }
QuestionRetriever >> sortQuestionsByVotes: questions [
	^ questions asSortedCollection: [ :a :b | a sizeOfPositiveVotes > b sizeOfPositiveVotes ].
]
