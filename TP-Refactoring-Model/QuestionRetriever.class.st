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

{ #category : #abstract }
QuestionRetriever >> getQuestionsFor: aUser [

	^ self subclassResponsibility.
]

{ #category : #private }
QuestionRetriever >> lastQuestions: questions [ 

	^ questions last: (100 min: questions size).
]

{ #category : #private }
QuestionRetriever >> rejectQuestionsWithAutor: aUser from: questions [ 

	^ questions reject: [ :q | q hasTheAuthor: aUser ].
]

{ #category : #'template ' }
QuestionRetriever >> retrieveQuestions: aUser [

	^ self retrieveQuestionsFor: aUser from: (self getQuestionsFor: aUser).
]

{ #category : #private }
QuestionRetriever >> retrieveQuestionsFor: aUser from: questions [
	| sortedQuestions |

	sortedQuestions := self sortQuestionsByVotes: questions.
	^ self rejectQuestionsWithAutor: aUser from: (self lastQuestions: sortedQuestions). 
	
]

{ #category : #private }
QuestionRetriever >> sortQuestionsByVotes: questions [

	^ questions asSortedCollection: [ :a :b | a hasMorePositiveVotesThan: b ].
	
]
