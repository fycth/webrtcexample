var RTCPeerConnection = null;
var getUserMedia = null;
var attachMediaStream = null;
var reattachMediaStream = null;
var webrtcDetectedBrowser = null;

function trace(text)
{
    if (text[text.length - 1] == '\n') text = text.substring(0, text.length - 1);
    console.log((performance.now() / 1000).toFixed(3) + ": " + text);
}

if (navigator.mozGetUserMedia)
{
    console.log("This appears to be Firefox");

    webrtcDetectedBrowser = "firefox";

    RTCPeerConnection = mozRTCPeerConnection;
    RTCSessionDescription = mozRTCSessionDescription;
    RTCIceCandidate = mozRTCIceCandidate;
    getUserMedia = navigator.mozGetUserMedia.bind(navigator);

    attachMediaStream =
        function(element, stream)
        {
            console.log("Attaching media stream");
            element.mozSrcObject = stream;
            element.play();
        };

    reattachMediaStream =
        function(to, from)
        {
            console.log("Reattaching media stream");
            to.mozSrcObject = from.mozSrcObject;
            to.play();
        };

    MediaStream.prototype.getVideoTracks =
        function()
        {
            return [];
        };

    MediaStream.prototype.getAudioTracks =
        function()
        {
            return [];
        };
}
else if (navigator.webkitGetUserMedia)
{
    console.log("This appears to be Chrome");

    webrtcDetectedBrowser = "chrome";

    RTCPeerConnection = webkitRTCPeerConnection;
    getUserMedia = navigator.webkitGetUserMedia.bind(navigator);
    attachMediaStream =
        function(element, stream)
        {  
            element.src = webkitURL.createObjectURL(stream);
        };

    reattachMediaStream =
        function(to, from)
        {
            to.src = from.src;
        };

    if (!webkitMediaStream.prototype.getVideoTracks)
    {
        webkitMediaStream.prototype.getVideoTracks =
            function()
            {
                return this.videoTracks;
            };
        webkitMediaStream.prototype.getAudioTracks =
            function()
            {
                return this.audioTracks;
            };
    }

    if (!webkitRTCPeerConnection.prototype.getLocalStreams)
    {
        webkitRTCPeerConnection.prototype.getLocalStreams =
            function()
            {
                return this.localStreams;
            };
        webkitRTCPeerConnection.prototype.getRemoteStreams =
            function()
            {
                return this.remoteStreams;
            };
    }
}
else
{
  console.log("Browser does not appear to be WebRTC-capable");
}
