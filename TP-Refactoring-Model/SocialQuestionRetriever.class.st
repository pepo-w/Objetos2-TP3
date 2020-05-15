Class {
	#name : #SocialQuestionRetriever,
	#superclass : #QuestionRetriever,
	#category : #'TP-Refactoring-Model'
}

{ #category : #retrieving }
SocialQuestionRetriever >> getQuestions: aUser [
	| followingCol |
	
	followingCol := OrderedCollection new.
	aUser following do: [ :follow | followingCol addAll: follow questions ].
	^ followingCol
]
