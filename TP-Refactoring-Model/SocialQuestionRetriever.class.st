Class {
	#name : #SocialQuestionRetriever,
	#superclass : #QuestionRetriever,
	#category : #'TP-Refactoring-Model'
}

{ #category : #retrieving }
SocialQuestionRetriever >> retrieveQuestions: aUser [
	| qRet temp followingCol |
	
	qRet := OrderedCollection new.
	followingCol := OrderedCollection new.
	aUser following do:[ :follow | followingCol addAll: follow questions ].
	temp := followingCol asSortedCollection:[ :a :b | a positiveVotes size > b positiveVotes size ].
	^ self retrieveQuestionsFor: aUser from: temp. 
]
