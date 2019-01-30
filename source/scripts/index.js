//  Initialise the Elm application
const mainNode = document.querySelector('main')
const main = window.Elm.Main.init({ node: mainNode })

// //  Constants
// const firebase = window.firebase

// //  Tell Elm whenever the connection state changes
// let connectionRef = firebase.database().ref('.info/connected')
// connectionRef.on('value', (snapshot) => {
//   if (snapshot.val() === true) {
//     console.log('Connected to firebase')
//   } else {
//     //  main.ports.userHasGoneOfflineFromJS.send(true)
//   }
// })
