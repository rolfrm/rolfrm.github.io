//import { connect } from 'nats.ws';

// Example usage
(async () => {
	 alert("connecting to nats")
  const nc = await connect({ servers: "ws://demo.nats.io:4222" });
  console.log("Connected to NATS");
})();
