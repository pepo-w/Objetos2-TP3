Class {
	#name : #NewsQuestionRetriever,
	#superclass : #QuestionRetriever,
	#category : #'TP-Refactoring-Model'
}

{ #category : #retrieving }
NewsQuestionRetriever >> getQuestionsFor: aUser [
	| newsCol |
	
	newsCol := OrderedCollection new.
	cuoora questions do:[:q | (q timestamp asDate = Date today) ifTrue: [newsCol add: q]]. "cuooraTodayQuestions"
	^ newsCol
]
