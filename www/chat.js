var token= getCookie('token')
var users = []

function acid(){
 var ws = createWebSocket(':9160/acid')
 ws.onopen = function(){ws.send(token)}
 ws.onmessage = function(event){JSONform(document.forms[0],JSON.parse(event.data))}
 ws.onclose = function() {}
}
acid()

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

//$(document).ready(function () {

function oooo(){

    var ws1 = createWebSocket(':9160/data')
    ws1.binaryType = 'blob'
    ws1.onopen = function(){
        dropBoxWS(ws1,document.getElementById('picture'))
        ws1.send(token)
        //var b=new Blob(['Facebook Code '+code],{"type":"text/plain"})
        //console.log(ws1.bufferedAmount)
    }
    ws1.onmessage = function(m){
        if (m.data instanceof ArrayBuffer) console.log("Type ArrayBuffer")
        if (m.data instanceof Blob){
            //if (!m.data.type.match(/image.*/)){return false}
            //m.data.type='image/png'
            preview(m.data,document.getElementById('picture'))
            //console.log("Type "+m.data.type)
        }
        if (typeof m.data === "string"){/*console.log("Type String")*/}
    }
    ws1.onclose = function() {$('#warnings').append("Connection 1 Closed ");}

    var ws2 = createWebSocket(':9160/chat');
    ws2.onopen = function(){ws2.send(token)}
    ws2.onmessage = function(event) {
        $('#warnings').html('');
        if (event.data.match('^Facebook Users ')){
            var str = event.data.replace(/^Facebook Users /, '');
            if(str != "") {
                users = str.split(", ");
                refreshUsers();
            }
            ws2.onmessage = onMessage;
			return true;
        }
        $('#warnings').append(event.data);
    };
    ws2.onclose = function() {$('#warnings').append("Connection 2 Closed ");}

}

//});

//$('#warnings').html('Connecting');
//ws.close
