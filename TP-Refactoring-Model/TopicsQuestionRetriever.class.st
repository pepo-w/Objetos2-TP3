Class {
	#name : #TopicsQuestionRetriever,
	#superclass : #QuestionRetriever,
	#category : #'TP-Refactoring-Model'
}

{ #category : #retrieving }
TopicsQuestionRetriever >> retrieveQuestions: aUser [
	| qRet temp topicsCol |
	
	qRet := OrderedCollection new.
	topicsCol := OrderedCollection new.
	aUser topics do:[ :topic | topicsCol addAll: topic questions ].
	temp := topicsCol asSortedCollection:[ :a :b | a positiveVotes size > b positiveVotes size ].
	^ self retrieveQuestionsFor: aUser from: temp. 
]
