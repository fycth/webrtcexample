/**
 * User: Andrey Sergienko
 * OSLIKAS http://www.oslikas.com
 * Date: 11/26/13
 * Time: 8:17 PM
 */

var WebRTCO = function() {
    // <WebRTC Adapter>
    var RTCPeerConnection = null;
    var getUserMedia = null;
    var attachMediaStream = null;
    var reattachMediaStream = null;
    var webrtcDetectedBrowser = null;
    //  </WebRTC Adapter>

    var debug = false;
    var room = null;
    var initiator;

    var localStream;
    var remoteStream;

    var pc = null;
    var signalingURL;

    var localVideo;
    var remoteVideo;

    var channelReady;
    var channel;

    var callbackReceiveChatMessage = null;
    var callbackOnRoomReceived = null;

    var pc_config = {"iceServers":
       [{url:'stun:23.21.150.121'},
        {url:'stun:stun.l.google.com:19302'},
        {url:'stun:stun01.sipphone.com'},
        {url:'stun:stun.ekiga.net'},
        {url:'stun:stun.fwdnet.net'},
        {url:'stun:stun.ideasip.com'},
        {url:'stun:stun.iptel.org'},
        {url:'stun:stun.rixtelecom.se'},
        {url:'stun:stun.schlund.de'},
        {url:'stun:stun.l.google.com:19302'},
        {url:'stun:stun1.l.google.com:19302'},
        {url:'stun:stun2.l.google.com:19302'},
        {url:'stun:stun3.l.google.com:19302'},
        {url:'stun:stun4.l.google.com:19302'},
        {url:'stun:stunserver.org'},
        {url:'stun:stun.softjoys.com'},
        {url:'stun:stun.voiparound.com'},
        {url:'stun:stun.voipbuster.com'},
        {url:'stun:stun.voipstunt.com'},
        {url:'stun:stun.voxgratia.org'},
        {url:'stun:stun.xten.com'},
        {
            url: 'turn:numb.viagenie.ca',
            credential: 'muazkh',
            username: 'webrtc@live.com'
        },
        {
            url: 'turn:192.158.29.39:3478?transport=udp',
            credential: 'JZEOEt2V3Qb0y27GRntt2u2PAYA=',
            username: '28224511:1379330808'
        },
        {
            url: 'turn:192.158.29.39:3478?transport=tcp',
            credential: 'JZEOEt2V3Qb0y27GRntt2u2PAYA=',
            username: '28224511:1379330808'
        }]};

    window.onbeforeunload = function() {
        sendMessage({type: 'bye'});
    };

    var sdpConstraints = {'mandatory': {'OfferToReceiveAudio':true, 'OfferToReceiveVideo':true }};

    var clog = function(str) {
        if (debug) { window.console.log(str); }
    };

    this.setDebug = function(b) {
        debug = b;
    };

    this.setCallBackReceiveChatMessage = function(callbackFunc) {
        callbackReceiveChatMessage = callbackFunc;
    };

    this.setCallOnRoomReceived = function(callbackFunc) {
        callbackOnRoomReceived = callbackFunc;
    };

    this.initialize = function(sURL, lv, rv) {
        clog("Initializing...");
        initWebRTCAdapter();
        signalingURL = sURL;
        localVideo = lv;
        remoteVideo = rv;
        openChannel();
    };

    var openChannel = function() {
        channelReady = false;
        channel = new WebSocket(signalingURL);
        channel.onopen = onChannelOpened;
        channel.onerror = onChannelError;
        channel.onmessage = onChannelMessage;
        channel.onclose = onChannelClosed;
    };

    var onChannelOpened = function() {
        clog('Channel opened...');
        channelReady = true;

        if(location.search.substring(1,5) == "room") {
            room = location.search.substring(6);
            sendMessage({"type" : "ENTERROOM", "value" : room * 1});
            initiator = true;
        } else {
            sendMessage({"type" : "GETROOM", "value" : ""});
            initiator = false;
        }
        doGetUserMedia();
    };

    var onChannelMessage = function(message) {
        clog('S->C: ' + message.data);
        processSignalingMessage(message.data);
    };

    var onChannelError = function() {
        clog('Channel error.');
    };

    var onChannelClosed = function() {
        clog('Channel closed.');
        channelReady = false;
    };

    var sendMessage = function(message) {
        var msgString = JSON.stringify(message);
        clog('C->S: ' + msgString);
        channel.send(msgString);
    };

    var processSignalingMessage = function(message) {
        var msg = JSON.parse(message);

        if (msg.type === 'CHATMSG') {
            if (null === callbackReceiveChatMessage) return;
            callbackReceiveChatMessage(msg.value);
        } else if (msg.type === 'offer') {
            pc.setRemoteDescription(new RTCSessionDescription(msg));
            doAnswer();
        } else if (msg.type === 'answer') {
            pc.setRemoteDescription(new RTCSessionDescription(msg));
        } else if (msg.type === 'candidate') {
            var candidate = new RTCIceCandidate({sdpMLineIndex:msg.label, candidate:msg.candidate});
            pc.addIceCandidate(candidate);
        } else if (msg.type === 'bye') {
            remoteHangup();
        } else if (msg.type === 'GETROOM') {
            room = msg.value;
            clog("received room number: " + room);
            if (null !== callbackOnRoomReceived) callbackOnRoomReceived(room);
        } else if (msg.type === 'WRONGROOM') {
            window.location.href = "/";
        }
    };

    this.chatSendMessage = function(chatMsg) {
        if (!channelReady) return;
        sendMessage({"type" : "CHATMSG", "value" : chatMsg});
    };

    var doGetUserMedia = function() {
        var constraints = {"audio": true, "video": {"mandatory": {}, "optional": []}};
        try {
            clog("Requested access to local media with mediaConstraints:\n \"" + JSON.stringify(constraints) + "\"");
            getUserMedia(constraints, onUserMediaSuccess, onUserMediaError);
        } catch (e) {
            clog("getUserMedia failed with exception: " + e.message);
        }
    };

    var onUserMediaSuccess = function(stream) {
        clog("User has granted access to local media.");
        attachMediaStream(localVideo, stream);
        localStream = stream;

        clog("Creating PeerConnection.");
        createPeerConnection();
        clog("Adding local stream.");
        pc.addStream(localStream);

        if (initiator) doCall();
    };

    var onUserMediaError = function(error) {
        clog("Failed to get access to local media. Error code was " + error.code);
    };

    var createPeerConnection = function() {
//        var pc_config = {"iceServers": [{"url": "stun:stun.l.google.com:19302"}]};
        var pc_constraints = {"optional": [{"DtlsSrtpKeyAgreement": true}]};

//        if (webrtcDetectedBrowser == "firefox") pc_config = {"iceServers":[{"url":"stun:23.21.150.121"}]};

        try {
            pc = new RTCPeerConnection(pc_config, pc_constraints);
            pc.onicecandidate = onIceCandidate;
            clog("Created RTCPeerConnnection with:\n" +
                "  config: \"" + JSON.stringify(pc_config) + "\";\n" +
                "  constraints: \"" + JSON.stringify(pc_constraints) + "\".");
        } catch (e) {
            pc = null;
            clog("Failed to create PeerConnection, exception: " + e.message);
            return;
        }
        pc.onaddstream = onRemoteStreamAdded;
        pc.onremovestream = onRemoteStreamRemoved;
    };

    var onIceCandidate = function(event) {
        if (event.candidate)
            sendMessage({type: 'candidate', label: event.candidate.sdpMLineIndex, id: event.candidate.sdpMid,
                candidate: event.candidate.candidate});

        else clog("End of candidates.");
    };

    var onRemoteStreamAdded = function(event) {
        clog("Remote stream added.");
        attachMediaStream(remoteVideo, event.stream);
        remoteStream = event.stream;
    };

    var onRemoteStreamRemoved = function(event) {
        clog("Remote stream removed.");
    };

    var doCall = function() {
        var constraints = {"optional": [], "mandatory": {"MozDontOfferDataChannel": true}};
        if (webrtcDetectedBrowser === "chrome")
            for (var prop in constraints.mandatory) if (prop.indexOf("Moz") != -1) delete constraints.mandatory[prop];

        constraints = mergeConstraints(constraints, sdpConstraints);
        clog("Sending offer to peer, with constraints: \n  \"" + JSON.stringify(constraints) + "\".")
        pc.createOffer(setLocalAndSendMessage, null, constraints);
    };

    var doAnswer = function() {
        clog("Sending answer to peer.");
        pc.createAnswer(setLocalAndSendMessage, null, sdpConstraints);
    };

    var remoteHangup = function() {
        clog('Session terminated.');
        pc.close();
    };

    var setLocalAndSendMessage = function(sessionDescription) {
        sessionDescription.sdp = preferOpus(sessionDescription.sdp);
        pc.setLocalDescription(sessionDescription);
        sendMessage(sessionDescription);
    };

    var mergeConstraints = function(cons1, cons2) {
        var merged = cons1;
        for (var name in cons2.mandatory) merged.mandatory[name] = cons2.mandatory[name];
        merged.optional.concat(cons2.optional);
        return merged;
    };

    // SDP stuff
    /**
     *
     * @param sdp
     * @returns {*}
     */
    var preferOpus = function(sdp) {
        var sdpLines = sdp.split('\r\n');

        for (var i = 0; i < sdpLines.length; i++) {
            if (sdpLines[i].search('m=audio') !== -1) {
                var mLineIndex = i;
                break;
            }
        }

        if (mLineIndex === null) return sdp;

        for (i = 0; i < sdpLines.length; i++) {
            if (sdpLines[i].search('opus/48000') !== -1) {
                var opusPayload = extractSdp(sdpLines[i], /:(\d+) opus\/48000/i);
                if (opusPayload) sdpLines[mLineIndex] = setDefaultCodec(sdpLines[mLineIndex], opusPayload);
                break;
            }
        }

        sdpLines = removeCN(sdpLines, mLineIndex);

        sdp = sdpLines.join('\r\n');
        return sdp;
    };

    var extractSdp = function(sdpLine, pattern) {
        var result = sdpLine.match(pattern);
        return (result && result.length == 2)? result[1]: null;
    };

    var setDefaultCodec = function(mLine, payload) {
        var elements = mLine.split(' ');
        var newLine = new Array();
        var index = 0;
        for (var i = 0; i < elements.length; i++) {
            if (index === 3) newLine[index++] = payload;
            if (elements[i] !== payload) newLine[index++] = elements[i];
        }
        return newLine.join(' ');
    };

    var removeCN = function(sdpLines, mLineIndex) {
        var mLineElements = sdpLines[mLineIndex].split(' ');
        for (var i = sdpLines.length-1; i >= 0; i--) {
            var payload = extractSdp(sdpLines[i], /a=rtpmap:(\d+) CN\/\d+/i);
            if (payload) {
                var cnPos = mLineElements.indexOf(payload);
                if (cnPos !== -1) mLineElements.splice(cnPos, 1);
                sdpLines.splice(i, 1);
            }
        }
        sdpLines[mLineIndex] = mLineElements.join(' ');
        return sdpLines;
    };

    /*
     * WebRTC Adapter
     */
    var initWebRTCAdapter = function() {
        if (navigator.mozGetUserMedia) {
            webrtcDetectedBrowser = "firefox";

            RTCPeerConnection = mozRTCPeerConnection;
            RTCSessionDescription = mozRTCSessionDescription;
            RTCIceCandidate = mozRTCIceCandidate;
            getUserMedia = navigator.mozGetUserMedia.bind(navigator);

            attachMediaStream =
                function(element, stream) {
                    element.mozSrcObject = stream;
                    element.play();
                };

            reattachMediaStream =
                function(to, from) {
                    to.mozSrcObject = from.mozSrcObject;
                    to.play();
                };

            MediaStream.prototype.getVideoTracks =
                function() {
                    return [];
                };

            MediaStream.prototype.getAudioTracks =
                function() {
                    return [];
                };
            return true;
        } else if (navigator.webkitGetUserMedia) {
            webrtcDetectedBrowser = "chrome";

            RTCPeerConnection = webkitRTCPeerConnection;
            getUserMedia = navigator.webkitGetUserMedia.bind(navigator);
            attachMediaStream =
                function(element, stream) {
                    element.src = webkitURL.createObjectURL(stream);
                };

            reattachMediaStream =
                function(to, from) {
                    to.src = from.src;
                };

            if (!webkitMediaStream.prototype.getVideoTracks) {
                webkitMediaStream.prototype.getVideoTracks =
                    function() {
                        return this.videoTracks;
                    };
                webkitMediaStream.prototype.getAudioTracks =
                    function() {
                        return this.audioTracks;
                    };
            }

            if (!webkitRTCPeerConnection.prototype.getLocalStreams) {
                webkitRTCPeerConnection.prototype.getLocalStreams =
                    function() {
                        return this.localStreams;
                    };
                webkitRTCPeerConnection.prototype.getRemoteStreams =
                    function() {
                        return this.remoteStreams;
                    };
            }
            return true;
        } else return false;
    };
};
