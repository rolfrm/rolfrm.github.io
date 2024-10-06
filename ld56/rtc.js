import { connect, credsAuthenticator } from "nats.ws" //"https://unpkg.com/nats.ws/nats.mjs";
let localConnection, remoteConnection, dataChannel, sendMessageBtn, messageInput, messagesDiv;

sendMessageBtn = document.getElementById('sendMessageBtn');
messageInput = document.getElementById('messageInput');
messagesDiv = document.getElementById('messages');

// Initialize WebRTC peer connections and data channel
function initWebRTC() {
  // Create two RTCPeerConnection instances (acting as peers in this demo)
  localConnection = new RTCPeerConnection();
  remoteConnection = new RTCPeerConnection();

  // Create a data channel on the local connection
  dataChannel = localConnection.createDataChannel("messageChannel");

  // Set up event listeners for the data channel
  dataChannel.onopen = () => console.log('Data channel is open');
  dataChannel.onclose = () => console.log('Data channel is closed');
  dataChannel.onmessage = receiveMessage;

  // When the remote connection receives a data channel, set its event listeners
  remoteConnection.ondatachannel = (event) => {
    let remoteChannel = event.channel;
    remoteChannel.onmessage = receiveMessage;
    remoteChannel.onopen = () => console.log('Remote data channel is open');
    remoteChannel.onclose = () => console.log('Remote data channel is closed');
  };

	
  // Set up ICE candidate exchange
	 localConnection.onicecandidate = (event) => {
		  console.log(event)
		if (event.candidate) {
			 
      remoteConnection.addIceCandidate(event.candidate);
    }
  };

	 remoteConnection.onicecandidate = (event) => {
		  console.log(event)
    if (event.candidate) {
      localConnection.addIceCandidate(event.candidate);
    }
  };

  // Create an offer to start the connection process
  localConnection.createOffer()
		  .then(offer => {
				console.log(offer)
      return localConnection.setLocalDescription(offer);
    })
		  .then(() => {
				console.log(localConnection.localDescription)
      return remoteConnection.setRemoteDescription(localConnection.localDescription);
    })
    .then(() => {
      return remoteConnection.createAnswer();
    })
		  .then(answer => {
				console.log(answer)
      return remoteConnection.setLocalDescription(answer);
    })
		  .then(() => {
				console.log(remoteConnection.localDescription)
      return localConnection.setRemoteDescription(remoteConnection.localDescription);
    })
    .catch(error => console.error('Error during WebRTC setup: ', error));
}

// Function to send a message over the data channel
function sendMessage() {
  const message = messageInput.value;
  if (dataChannel.readyState === 'open') {
    dataChannel.send(message);
    displayMessage(`Sent: ${message}`);
  }
}

function sendMessage2(message) {
  if (dataChannel.readyState === 'open') {
    dataChannel.send(message);
    displayMessage(`Sent: ${message}`);
  }
}

// Function to handle incoming messages
function receiveMessage(event) {
  const message = event.data;
  displayMessage(`Received: ${message}`);
}

// Utility function to display messages
function displayMessage(message) {
  const messageElem = document.createElement('div');
  messageElem.textContent = message;
  messagesDiv.appendChild(messageElem);
}

// Attach event listener for sending messages
//sendMessageBtn.addEventListener('click', sendMessage);

// Initialize WebRTC
//initWebRTC();

//let s1 = "wss://connect.ngs.global";
//let s1 = "wss://demo.nats.io:8443"
 //const authenticator = credsAuthenticator(creds);
async function setupNATS() {
	 const nc = await connect({ servers: s1, authenticator: authenticator });  console.log("Connected to NATS and listening for messages");
	 nc.publish("foo.bar", "123")
	 nc.publish("foo.bar", "1234")
	 nc.publish("foo.bar", "12345")
	 const chat = await nc.subscribe("foo.*");
	 for await (const m of chat) {
		  console.log(m)
	 }
}

//nats = setupNATS

//setupNATS();
