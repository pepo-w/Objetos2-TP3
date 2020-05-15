Class {
	#name : #PopularTodayQuestionRetriever,
	#superclass : #QuestionRetriever,
	#category : #'TP-Refactoring-Model'
}

{ #category : #retrieving }
PopularTodayQuestionRetriever >> getQuestions: aUser [
	| popularTCol averageVotes |
	
	popularTCol := OrderedCollection new.
	averageVotes := (cuoora questions sum: [:q | q positiveVotes size ]) / popularTCol size.
	cuoora questions do:[:q | (q timestamp asDate = Date today) ifTrue: [popularTCol add: q]]. "cuooraTodayQuestions"
	^ (popularTCol select: [:q | q positiveVotes size >= averageVotes ]) "cuoora questionsAboveAverage"
	
]
