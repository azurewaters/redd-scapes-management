//  Initialise the Elm application
const mainNode = document.querySelector('main')
const main = window.Elm.Main.init({ node: mainNode })

//  Add incoming port listeners
main.ports.deleteThisMeeting.subscribe(deleteThisMeeting)
main.ports.fetchMeetings.subscribe(fetchMeetings)
main.ports.signOut.subscribe(signOut)
main.ports.verifyCredentials.subscribe(verifyCredentials)

// //  Constants
const firebase = window.firebase

//  Add firebase listeners
firebase.auth().onAuthStateChanged(authStateChanged)

function authStateChanged (user) {
  if (user) {
    //  User is signed in
    let userToSendToElm =
            { uid: user.uid,
              email: user.email
            }
    main.ports.authenticationSucceededFromJS.send(userToSendToElm)
  } else {
    //  User has signed out
    main.ports.authenticatedUserSignedOutFromJS.send(true)
  }
}

function deleteThisMeeting (meetingId) {
  firebase
    .firestore()
    .collection('meetings')
    .doc(meetingId)
    .delete()
    .then(() => {
      //  Let Elm know
      main.ports.meetingWasDeletedFromJS.send(meetingId)

      //  Now, delete all the meeting's guests
      firebase
        .firestore()
        .collection('guests')
        .where('meetingId', '==', meetingId)
        .get()
        .then((snapshot) => {
          snapshot.forEach((documentSnapshot) => {
            documentSnapshot.ref.delete()
          })
        })
        .then(() => {
          console.log('all done')
        })
    })
    .catch((error) => {
      console.log(error)
    })
}

function fetchMeetings () {
  firebase
    .firestore()
    .collection('meetings')
    .get()
    .then((snapshot) => {
      let meetings = []
      snapshot.forEach((doc) => {
        let data = doc.data()

        let meeting = {}
        meeting.id = doc.id
        meeting.name = data.name
        meeting.date = data.date
        meeting.startTime = data.startTime
        meeting.endTime = data.endTime

        meetings.push(meeting)
      })
      main.ports.meetingsFromJS.send(meetings)
    })
}

function signOut (data) {
  firebase.auth().signOut()
}

function verifyCredentials (data) {
  //  We verify this against the database and return a User object
  firebase.auth()
    .signInWithEmailAndPassword(data.login, data.password)
    .catch((error) => {
      console.log('An error occurred while authenticating', error)
      main.ports.authenticationFailedFromJS.send(error.code)
    })
}
