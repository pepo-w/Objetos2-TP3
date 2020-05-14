Class {
	#name : #Vote,
	#superclass : #Object,
	#instVars : [
		'isLike',
		'timstamp',
		'publication',
		'user'
	],
	#category : #'TP-Refactoring-Model'
}

{ #category : #'instance creation' }
Vote class >> user: aUser dislikesPublication: aPublication [
	^ self new
		initWithUser: aUser publication: aPublication;
		dislike;
		yourself
]

{ #category : #'instance creation' }
Vote class >> user: aUser likesPublication: aPublication [
	^ self new
		initWithUser: aUser publication: aPublication;
		yourself
]

{ #category : #private }
Vote >> dislike [
	isLike := false.
]

{ #category : #initialization }
Vote >> initWithUser: anUser publication: aPublication [
	user := anUser.
	publication := aPublication
]

{ #category : #initialize }
Vote >> initialize [
	isLike := true.
	timstamp := DateAndTime now.
	
]

{ #category : #accessing }
Vote >> isLike [
	^ isLike
]

{ #category : #private }
Vote >> like [
	isLike := true.
]

{ #category : #accessing }
Vote >> publication [
	^ publication
]

{ #category : #accessing }
Vote >> user [
	^ user
]
