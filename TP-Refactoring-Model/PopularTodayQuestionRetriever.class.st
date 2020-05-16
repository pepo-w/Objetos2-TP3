Class {
	#name : #PopularTodayQuestionRetriever,
	#superclass : #QuestionRetriever,
	#category : #'TP-Refactoring-Model'
}

{ #category : #retrieving }
PopularTodayQuestionRetriever >> getQuestionsFor: aUser [
	| popularTCol averageVotes |
	
	popularTCol := OrderedCollection new.
	cuoora questions do:[:q | (q timestamp asDate = Date today) ifTrue: [popularTCol add: q]]. "cuoora todayQuestions"
	averageVotes := (cuoora questions sum: [:q | q positiveVotes size ]) / popularTCol size.
	^ (popularTCol select:[:q | q positiveVotes size >= averageVotes ]) "cuoora questionsAboveAverage"
	

]
