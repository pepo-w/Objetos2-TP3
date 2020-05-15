Class {
	#name : #PopularTodayQuestionRetriever,
	#superclass : #QuestionRetriever,
	#category : #'TP-Refactoring-Model'
}

{ #category : #retrieving }
PopularTodayQuestionRetriever >> getQuestionsFor: aUser [
	| popularTCol averageVotes |
	
	popularTCol := OrderedCollection new.
	cuoora questions do:[:q | (q timestamp asDate = Date today) ifTrue: [popularTCol add: q]]. "cuooraTodayQuestions"
	averageVotes := (cuoora questions sum: [:q | q positiveVotes size ]) / popularTCol size.
	^ (popularTCol select:[:q | q positiveVotes size >= averageVotes ]) "cuoora questionsAboveAverage"
	
"PopularTodayQuestionRetriever>>retrieveQuestions: aUser
	| qRet temp popularTCol averageVotes |
	
	qRet := OrderedCollection new.
	popularTCol := OrderedCollection new.
	cuoora questions do:[:q | (q timestamp asDate = Date today) ifTrue: [popularTCol add: q]].
	averageVotes := (cuoora questions sum: [:q | q positiveVotes size ]) / popularTCol size.
	temp := (popularTCol select:[:q | q positiveVotes size >= averageVotes ]) asSortedCollection:[ :a :b | a positiveVotes size > b positiveVotes size ].
	qRet := temp last: (100 min: temp size).
	
	^qRet reject:[:q | q user = aUser]."
]
