  var wsURI = 'ws://www.webrtcexample.com/s';

  var localVideo;
  var remoteVideo;
  var localStream;
  var remoteStream;
  var channelReady = false;
  var connection;
  var pc;
  var room;
  var initiator = 0;
  var started = false;
  var roulette = false;

  var sdpConstraints = {'mandatory': {'OfferToReceiveAudio':true, 'OfferToReceiveVideo':true }};
  var isVideoMuted = false;
  var isAudioMuted = false;

  function initialize()
  {
      console.log("Initializing;");
      localVideo = document.getElementById("localVideo");
      remoteVideo = document.getElementById("remoteVideo");
      resetStatus();
      openChannel();
      doGetUserMedia();
  }

  function init_room()
  {
      roulette = false;
      initialize();
  }

  function init_roulette()
  {
      roulette = true;
      initialize();
  }

  function openChannel()
  {
      connection = new WebSocket(wsURI);
      connection.onopen = onChannelOpened;
      connection.onerror = function (error) { console.log('WebSocket Error ' + error); };
      connection.onmessage = onChannelMessage;
      connection.onclose = onChannelClosed;
  }

  function resetStatus()
  {
      if (!initiator)
      {
          if (!roulette) setStatus("Waiting for someone to join: <a href=\""+window.location.href+"?room="+room+"\">"+window.location.href+"?room="+room+"</a>");
          else setStatus("Waiting for someone");
      }
      else setStatus("Initializing...");
  }

  function doGetUserMedia()
  {
      var constraints = {"audio": true, "video": {"mandatory": {}, "optional": []}}; 
      try
      {
          getUserMedia(constraints, onUserMediaSuccess, onUserMediaError);
          console.log("Requested access to local media with mediaConstraints:\n \"" + JSON.stringify(constraints) + "\"");
      }
      catch (e)
      {
          alert("getUserMedia() failed. Is this a WebRTC capable browser?");
          console.log("getUserMedia failed with exception: " + e.message);
      }
  }

  function createPeerConnection()
  {
      var pc_config = {"iceServers": [{"url": "stun:stun.l.google.com:19302"}]};
      var pc_constraints = {"optional": [{"DtlsSrtpKeyAgreement": true}]};

      if (webrtcDetectedBrowser == "firefox") pc_config = {"iceServers":[{"url":"stun:23.21.150.121"}]};

      try
      {
          pc = new RTCPeerConnection(pc_config, pc_constraints);
          pc.onicecandidate = onIceCandidate;
          console.log("Created RTCPeerConnnection with:\n" + 
                  "  config: \"" + JSON.stringify(pc_config) + "\";\n" + 
                  "  constraints: \"" + JSON.stringify(pc_constraints) + "\".");
      }
      catch (e)
      {
          console.log("Failed to create PeerConnection, exception: " + e.message);
          alert("Cannot create RTCPeerConnection object; WebRTC is not supported by this browser.");
          return;
      }
      pc.onaddstream = onRemoteStreamAdded;
      pc.onremovestream = onRemoteStreamRemoved;
  }

  function maybeStart()
  {
      if (!started && localStream && channelReady)
      {
          setStatus("Connecting...");
          console.log("Creating PeerConnection.");
          createPeerConnection();
          console.log("Adding local stream.");
          pc.addStream(localStream);
          started = true;

          if (initiator) doCall();
      }
  }

  function setStatus(state) { footer.innerHTML = state; }

  function doCall()
  {
      var constraints = {"optional": [], "mandatory": {"MozDontOfferDataChannel": true}};
      if (webrtcDetectedBrowser === "chrome")
          for (prop in constraints.mandatory) if (prop.indexOf("Moz") != -1) delete constraints.mandatory[prop];

      constraints = mergeConstraints(constraints, sdpConstraints);
      console.log("Sending offer to peer, with constraints: \n  \"" + JSON.stringify(constraints) + "\".")
      pc.createOffer(setLocalAndSendMessage, null, constraints);
  }

  function doAnswer()
  {
      console.log("Sending answer to peer.");
      pc.createAnswer(setLocalAndSendMessage, null, sdpConstraints);
  }

  function mergeConstraints(cons1, cons2)
  {
      var merged = cons1;
      for (var name in cons2.mandatory) merged.mandatory[name] = cons2.mandatory[name];
      merged.optional.concat(cons2.optional);
      return merged;
  }

  function setLocalAndSendMessage(sessionDescription)
  {
      sessionDescription.sdp = preferOpus(sessionDescription.sdp);
      pc.setLocalDescription(sessionDescription);
      sendMessage(sessionDescription);
  }

  function sendMessage(message)
  {
      var msgString = JSON.stringify(message);
      console.log('C->S: ' + msgString);
      connection.send(msgString);
  }

  function chatSendMessage(msg)
  {
      sendMessage({"type" : "CHATMSG", "value" : msg});
  }

  function processSignalingMessage(message)
  {
      var msg = JSON.parse(message);

      if (msg.type === 'ROULETTE')
      {
          if (msg.value === 'go') 
          {
              console.log('got reply: we are an initiator');
              initiator = 1;
              maybeStart();
          }
          else
          {
              console.log("got reply: we are waiting");
              initiator = 0;
          }
      }
      else if (msg.type === 'CHATMSG')
      {
          addChatRMsg(msg.value);
      }
      else if (msg.type === 'offer')
      {
          if (!initiator && !started) maybeStart();

          pc.setRemoteDescription(new RTCSessionDescription(msg));
          doAnswer();
      }
      else if (msg.type === 'answer' && started)
      {
          pc.setRemoteDescription(new RTCSessionDescription(msg));
      }
      else if (msg.type === 'candidate' && started)
      {
          var candidate = new RTCIceCandidate({sdpMLineIndex:msg.label, candidate:msg.candidate});
          pc.addIceCandidate(candidate);
      }
      else if (msg.type === 'bye' && started)
      {
          onRemoteHangup();
      }
      else if (msg.type === 'GETROOM')
      {
          room = msg.value;
          console.log("received room number: " + room);
          resetStatus();
          initiator = false;
      }
      else if (msg.type === 'WRONGROOM')
      {
          window.location.href="/";
      }
  }

  function onChannelOpened()
  {
      console.log('Channel opened.');
      channelReady = true;

      if (roulette)
      {
          console.log("Sending roulette request");
          sendMessage({"type" : "ROULETTE", "value" : ""});
          initiator = 0;
      }
      else 
      if(location.search.substring(1,5) == "room")
      {
          room = location.search.substring(6);
          sendMessage({"type" : "INVITE", "value" : room});
          initiator = 1;
      }
      else
      {
          sendMessage({"type" : "GETROOM", "value" : ""});
          initiator = 0;
      }
      if (initiator) maybeStart();
  }

  function onChannelMessage(message)
  {
      console.log('S->C: ' + message.data);
      processSignalingMessage(message.data);
  }

  function onChannelError()
  {
      console.log('Channel error.');
  }

  function onChannelClosed()
  {
      console.log('Channel closed.');
      channelReady = false;
  }

  function onUserMediaSuccess(stream)
  {
      console.log("User has granted access to local media.");
      attachMediaStream(localVideo, stream);
      localStream = stream;
      if (initiator) maybeStart();
  }

  function onUserMediaError(error)
  {
      console.log("Failed to get access to local media. Error code was " + error.code);
      alert("Failed to get access to local media. Error code was " + error.code + ".");
  }

  function onIceCandidate(event)
  {
      if (event.candidate)
          sendMessage({type: 'candidate', label: event.candidate.sdpMLineIndex, id: event.candidate.sdpMid,
                   candidate: event.candidate.candidate});

      else console.log("End of candidates.");
  }

  function onRemoteStreamAdded(event)
  {
      console.log("Remote stream added."); 
      attachMediaStream(remoteVideo, event.stream);
      remoteStream = event.stream;

      setStatus("Connected!");

//      disableButtons();
  }

  function onRemoteStreamRemoved(event)
  {
      console.log("Remote stream removed.");
  }

  function onHangup()
  {
      console.log("Hanging up.");
      stop();
      connection.close();
//      enableButtons();
      setStatusHangup();
  }
   
  function onRemoteHangup()
  {
      console.log('Session terminated.');
      stop();
      initiator = 0;
      setStatusHangup();
  }

  function setStatusHangup()
  {
      setStatus("Connection has been closed, but you can create a new conference.");
  }

  function stop()
  {
      started = false;
      isAudioMuted = false;
      isVideoMuted = false;
      pc.close();
      pc = null;
  }

  function toggleVideoMute()
  {
      videoTracks = localStream.getVideoTracks();

      if (videoTracks.length === 0)
      {
          console.log("No local video available.");
          return;
      }

      if (isVideoMuted)
      {
          for (i = 0; i < videoTracks.length; i++) videoTracks[i].enabled = true;
          console.log("Video unmuted.");
      }
      else
      {
          for (i = 0; i < videoTracks.length; i++) videoTracks[i].enabled = false;
          console.log("Video muted.");
      }

      isVideoMuted = !isVideoMuted;    
  }

  function toggleAudioMute()
  {
      audioTracks = localStream.getAudioTracks();

      if (audioTracks.length === 0)
      {
          console.log("No local audio available.");
          return;
      }

      if (isAudioMuted)
      {
          for (i = 0; i < audioTracks.length; i++) audioTracks[i].enabled = true;
          console.log("Audio unmuted.");
      }
      else
      {
          for (i = 0; i < audioTracks.length; i++) audioTracks[i].enabled = false;
          console.log("Audio muted.");
      }

      isAudioMuted = !isAudioMuted;  
  }

  // Send BYE on refreshing(or leaving) a demo page
  // to ensure the room is cleaned for next session.
  window.onbeforeunload = function()
  {
    sendMessage({type: 'bye'});
  }

  // Ctrl-D: toggle audio mute; Ctrl-E: toggle video mute.
  // On Mac, Command key is instead of Ctrl.
  // Return false to screen out original Chrome shortcuts.
  document.onkeydown = function()
  {
      if (navigator.appVersion.indexOf("Mac") != -1)
      {
          if (event.metaKey && event.keyCode === 68)
          {
              toggleAudioMute();
              return false;
          }
          if (event.metaKey && event.keyCode === 69)
          {
              toggleVideoMute();
              return false;
          }
      }
      else
      {
          if (event.ctrlKey && event.keyCode === 68)
          {
              toggleAudioMute();
              return false;
          }
          if (event.ctrlKey && event.keyCode === 69)
          {
              toggleVideoMute();
              return false;
          }
      }
      return false;
  }

  function preferOpus(sdp)
  {
      var sdpLines = sdp.split('\r\n');

      // Search for m line.
      for (var i = 0; i < sdpLines.length; i++)
      {
          if (sdpLines[i].search('m=audio') !== -1)
          {
              var mLineIndex = i;
              break;
          } 
      }
      
      if (mLineIndex === null) return sdp;

      // If Opus is available, set it as the default in m line.
      for (i = 0; i < sdpLines.length; i++)
      {
          if (sdpLines[i].search('opus/48000') !== -1)
          {
              var opusPayload = extractSdp(sdpLines[i], /:(\d+) opus\/48000/i);
              if (opusPayload) sdpLines[mLineIndex] = setDefaultCodec(sdpLines[mLineIndex], opusPayload);
              break;
          }
      }

      // Remove CN in m line and sdp.
      sdpLines = removeCN(sdpLines, mLineIndex);

      sdp = sdpLines.join('\r\n');
      return sdp;
  }

  function extractSdp(sdpLine, pattern)
  {
      var result = sdpLine.match(pattern);
      return (result && result.length == 2)? result[1]: null;
  }

  // Set the selected codec to the first in m line.
  function setDefaultCodec(mLine, payload)
  {
      var elements = mLine.split(' ');
      var newLine = new Array();
      var index = 0;
      for (var i = 0; i < elements.length; i++)
      {
          // Format of media starts from the fourth.
          if (index === 3) newLine[index++] = payload; // Put target payload to the first.
          if (elements[i] !== payload) newLine[index++] = elements[i];
      }
      return newLine.join(' ');
  }

  // Strip CN from sdp before CN constraints is ready.
  function removeCN(sdpLines, mLineIndex)
  {
      var mLineElements = sdpLines[mLineIndex].split(' ');
      // Scan from end for the convenience of removing an item.
      for (var i = sdpLines.length-1; i >= 0; i--)
      {
          var payload = extractSdp(sdpLines[i], /a=rtpmap:(\d+) CN\/\d+/i);
          if (payload)
          {
              var cnPos = mLineElements.indexOf(payload);

              // Remove CN payload from m line.
              if (cnPos !== -1) mLineElements.splice(cnPos, 1);

              // Remove CN line in sdp
              sdpLines.splice(i, 1);
          }
      }

      sdpLines[mLineIndex] = mLineElements.join(' ');
      return sdpLines;
  }
