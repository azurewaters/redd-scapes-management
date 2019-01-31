//  Initialise the Elm application
const mainNode = document.querySelector('main')
const main = window.Elm.Main.init({ node: mainNode })

//  Add incoming port listeners
main.ports.verifyCredentials.subscribe(verifyCredentials)
main.ports.signOut.subscribe(signOut)

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

function verifyCredentials (data) {
  //  We verify this against the database and return a User object
  firebase.auth()
    .signInWithEmailAndPassword(data.login, data.password)
    .catch((error) => {
      console.log('An error occurred while authenticating', error)
      main.ports.authenticationFailedFromJS.send(error.code)
    })
}

function signOut (data) {
  firebase.auth().signOut()
}
