Class {
	#name : #Publication,
	#superclass : #Object,
	#instVars : [
		'timestamp',
		'user',
		'votes',
		'description'
	],
	#category : #'TP-Refactoring-Model'
}

{ #category : #adding }
Publication >> addVote: aVote [
	votes add: aVote
]

{ #category : #accessing }
Publication >> description [
	^ description
]

{ #category : #private }
Publication >> description: anObject [
	description := anObject
]

{ #category : #boolean }
Publication >> hasMorePositiveVotesThan: aPublication [
	^ self sizeOfPositiveVotes > aPublication sizeOfPositiveVotes.
]

{ #category : #boolean }
Publication >> hasTheAuthor: aUser [
	^ self user = aUser.
]

{ #category : #initialize }
Publication >> initialize [
	votes := OrderedCollection new.
	timestamp := DateAndTime now.
]

{ #category : #boolean }
Publication >> isFromToday [
	^ self timestamp asDate = Date today.
]

{ #category : #'collection getters' }
Publication >> negativeVotes [
	^ votes reject: [ :vote | vote isLike ].
]

{ #category : #'collection getters' }
Publication >> positiveVotes [
	^ votes select: [ :vote | vote isLike ].
]

{ #category : #calculating }
Publication >> sizeOfPositiveVotes [
	^ self positiveVotes size.
]

{ #category : #accessing }
Publication >> timestamp [
	^ timestamp
]

{ #category : #private }
Publication >> timestamp: anObject [
	timestamp := anObject
]

{ #category : #accessing }
Publication >> user [
	^ user
]

{ #category : #private }
Publication >> user: anObject [
	user := anObject
]

{ #category : #accessing }
Publication >> votes [
	^ votes
]
