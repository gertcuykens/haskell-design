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

//$(document).ready(function () {
    try{var code = document.URL.match('code=.*')[0].replace(/^.*code=/,'')}
    catch(e){var code = ''}

    var ws0 = createWebSocket(':9160/acid')
    ws0.onopen = function(){ws0.send('Facebook Code '+ code)}
    ws0.onmessage = function(event){
        if (event.data.match('^Facebook Login ')){
            document.location=event.data.match('https.*')+'&state=chat'
            return true
        }
        if (event.data.match('^Facebook Uid ')){return true}
        //console.log(event.data)
        JSONform(document.forms[0],JSON.parse(event.data))
    }
    ws0.onclose = function() {$('#warnings').append("Connection 0 Closed ");} 

    var ws1 = createWebSocket(':9160/data')
    ws1.binaryType = 'blob'
    ws1.onopen = function(){
        dropBoxWS(ws1,document.getElementById('picture'))
        ws1.send('Facebook Code '+code)
        //var b=new Blob(['Facebook Code '+code],{"type":"text/plain"})
        //console.log(ws1.bufferedAmount)
    }
    ws1.onmessage = function(m){
        if (m.data instanceof ArrayBuffer) console.log("Type ArrayBuffer")
        if (m.data instanceof Blob){
            //if (!m.data.type.match(/image.*/)){return false}
            //m.data.type='image/png'
            preview(m.data,document.getElementById('picture'))
            console.log("Type "+m.data.type)
        }
        if (typeof m.data === "string"){
            if (m.data.match('^Facebook Login ')){
                document.location=m.data.match('https.*')+'&state=chat'
                return true
            }
            console.log("Type String")
        }
    }
    ws1.onclose = function() {$('#warnings').append("Connection 1 Closed ");} 

    var ws2 = createWebSocket(':9160/chat');
    ws2.onopen = function(){ws2.send('Facebook Code ' + code)}
    ws2.onmessage = function(event) {
        $('#warnings').html('');
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
            ws2.onmessage = onMessage;
			return true;
        }
        $('#warnings').append(event.data);
    };
    ws2.onclose = function() {$('#warnings').append("Connection 2 Closed ");} 

//});

//$('#warnings').html('Connecting');
//ws.close
