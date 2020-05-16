Class {
	#name : #SocialQuestionRetriever,
	#superclass : #QuestionRetriever,
	#category : #'TP-Refactoring-Model'
}

{ #category : #retrieving }
SocialQuestionRetriever >> getQuestionsFor: aUser [
	| followingCol |
	
	followingCol := OrderedCollection new.
	aUser following do: [ :follow | followingCol addAll: follow questions ]. "aUser followingQuestions"
	^ followingCol
]
