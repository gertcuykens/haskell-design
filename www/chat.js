function createWebSocket(path) {
    var host = window.location.hostname;
    if(host == '') host = 'localhost';
    var uri = 'ws://' + host + path;
    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    return new Socket(uri);
}

var users = [];

function refreshUsers() {
    $('#users').html('');
    for(i in users) {
        $('#users').append($(document.createElement('li')).text(users[i]));
    }
}

function onMessage(event) {
    var p = $(document.createElement('p')).text(event.data);
    $('#messages').append(p);
    $('#messages').animate({scrollTop: $('#messages')[0].scrollHeight});
    if(event.data.match(/^[^:]* joined/)) {
        var user = event.data.replace(/ joined/, '');
        users.push(user);
        refreshUsers();
    }
    if(event.data.match(/^[^:]* disconnected/)) {
        var user = event.data.replace(/ disconnected/, '');
        var idx = users.indexOf(user);
        users = users.slice(0, idx).concat(users.slice(idx + 1));
        refreshUsers();
    }
}

$(document).ready(function () {
    $('#join-form').submit(function () {
        $('#join-section').hide();
        $('#warnings').html('Connecting');
        var ws = createWebSocket(':9160');
        ws.onopen = function() {ws.send('Facebook Code ' + code)}
        ws.onmessage = function(event) {
            if (event.data.match('^Facebook Login ')){
                document.location=event.data.match('https.*')+'&state=chat'
                return true
            }
            if (event.data.match('^Facebook Users ')){
                var str = event.data.replace(/^Facebook Users /, '');
                if(str != "") {
                    users = str.split(", ");
                    refreshUsers();
                }
                $('#warnings').html('');
                $('#join-section').hide();
                $('#chat-section').show();
                $('#users-section').show();
                ws.onmessage = onMessage;
                $('#message-form').submit(function () {
                    var text = $('#text').val();
                    ws.send(text);
                    $('#text').val('');
                });
				return true;
            }
            $('#warnings').append(event.data);
            ws.close();
        };
    });
    try{var code = document.URL.match('code=.*')[0].replace(/^.*code=/,'');$('#join-form').submit()}
    catch(e){var code = 'facebook';}
});
