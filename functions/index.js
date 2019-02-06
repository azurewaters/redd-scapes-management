const functions = require('firebase-functions')

// exports.deleteGuestsWhenAMeetingIsDeleted =
// functions.firestore
//   .document('meetings/{meetingId}')
//   .onDelete((snapshot, context) => {
//     //  This is where we delete all the guests that were to attend this meeting
//     const firebase = require('firebase-admin')
//     firebase.initializeApp(functions.config().firebase)

//     //  Now, delete all the guests with that meetingId
//     return firebase
//       .firestore()
//       .collection('guests')
//       .where('meetingId', '==', context.params.meetingId)
//       .get()
//       .then((snapshot) => {
//         snapshot.forEach((childSnapshot) => {
//           childSnapshot.ref.delete()
//         })
//       })
//   })
