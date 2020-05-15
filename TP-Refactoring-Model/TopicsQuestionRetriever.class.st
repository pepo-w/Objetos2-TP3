Class {
	#name : #TopicsQuestionRetriever,
	#superclass : #QuestionRetriever,
	#category : #'TP-Refactoring-Model'
}

{ #category : #retrieving }
TopicsQuestionRetriever >> getQuestionsFor: aUser [
	| topicsCol |
	
	topicsCol := OrderedCollection new.
	aUser topics do: [ :topic | topicsCol addAll: topic questions ].
	^ topicsCol
]
