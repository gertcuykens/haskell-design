var token= getCookie('token')

var c = document.URL.match('code=.*')
if (c instanceof Array){
    c=c[0].replace(/^.*code=/,'')
    var code = new WebSocket('ws:localhost:9160/code')
    code.onopen = function(){code.send(c)}
    code.onmessage = function(e){token=e.data}
    code.onclose = function(){main(token)}
}else{
    if (token) main(token)
    //var state = document.URL.match('state=.*&')
    //if (!(state instanceof Array)){state=['state=chat&']}
    //state=state[0].substring(6,state[0].indexOf('&'))
}

function main(token){

    setCookie("token",token,1)

    acid = new WebSocket('ws://localhost:9160/acid')
    acid.onopen = function(){acid.send(token)}
    acid.onmessage = function(event){JSONform(document.forms[0],JSON.parse(event.data))}
    acid.onclose = function() {$('#warnings').append("Acid closed ");}

    data = new WebSocket('ws://localhost:9160/data')
    data.binaryType = 'blob'
    data.onopen = function(){
        dropBoxWS(data,document.getElementById('picture'))
        data.send(token)
        //var b=new Blob(['Facebook Code '+code],{"type":"text/plain"})
        //console.log(ws1.bufferedAmount)
    }
    data.onmessage = function(m){
        if (m.data instanceof ArrayBuffer) console.log("Type ArrayBuffer")
        if (m.data instanceof Blob){
            //if (!m.data.type.match(/image.*/)){return false}
            //m.data.type='image/png'
            preview(m.data,document.getElementById('picture'))
            //console.log("Type "+m.data.type)
        }
        if (typeof m.data === "string"){/*console.log("Type String")*/}
    }
    data.onclose = function() {$('#warnings').append("Data closed ");}

    var users = []

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

    chat = new WebSocket('ws://localhost:9160/chat');
    chat.onopen = function(){chat.send(token)}
    chat.onmessage = function(event) {
        $('#warnings').html('');
        if (event.data.match('^Facebook Users ')){
            var str = event.data.replace(/^Facebook Users /, '');
            if(str != "") {
                users = str.split(", ");
                refreshUsers();
            }
            chat.onmessage = onMessage;
            return true;
        }
        $('#warnings').append(event.data);
    };
    chat.onclose = function() {$('#warnings').append("Chat closed ");}

}
