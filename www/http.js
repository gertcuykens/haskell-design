xhr=function(o){
 var statBox=document.createElement('progress')
 statBox.value=0
 statBox.max=100
 statBox.style.display='none'
 statBox.addEventListener('click',function(e){this.abort()},false)
 document.body.appendChild(statBox)
 var x=new XMLHttpRequest()
 x.addEventListener('loadstart',function(e){
  statBox.style.display='inline-block'
  statBox.style.backgroundColor='green'
  statBox.value=0
 },false)
 x.addEventListener('abort',function(e){statBox.style.display='none'},false)
 x.addEventListener('progress',function(e){if(e.lengthComputable)statBox.value=Math.round((e.loaded*100)/e.total)},false)
 x.addEventListener('error',function(e){statBox.style.backgroundColor='red'},false)
 x.addEventListener('load',function(e){
  statBox.style.display='none'
  if(this.getResponseHeader('ETag')){o.ETag=this.getResponseHeader('ETag');return 0}
  if(this.getResponseHeader('X-Couch-Update-NewRev')){o.ETag=this.getResponseHeader('X-Couch-Update-NewRev');return 0}
 },false)
 return x
}

formURI=function(v){
 var t=v.getElementsByTagName('input')
 var s=''
 for(i in t)if(t[i].type=='text')s+=encodeURIComponent(t[i].name)+'='+encodeURIComponent(t[i].value)+'&'
 return s.slice(0,-1)
}

formJSON=function(v){
 var t=v.getElementsByTagName('input')
 var s=new Object()
 for(i in t)if(t[i].type=='text')s[t[i].name]=t[i].value
 return s
}

JSONform=function(v,j){for(i in j)try{v[i].value=j[i]}catch(e){}}

function createWebSocket(path) {
    var host = window.location.hostname;
    if(host == '') host = 'localhost';
    var uri = 'ws://' + host + path;
    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    return new Socket(uri);
}

preview=function(f,d){
 if (!f.type.match(/image.*/))return false
 var reader=new FileReader()
 //reader.addEventListener('load',(function(i){return function(e){i.src=e.target.result}})(d),false)
 reader.onload=(function(i){return function(e){i.src=e.target.result}})(d)
 reader.readAsDataURL(f)
}

dropBox=function(d){
 var dragenter=function(e){e.stopPropagation();e.preventDefault()}
 var dragover=function(e){e.stopPropagation();e.preventDefault()}
 var drop=function(e){
  e.stopPropagation()
  e.preventDefault()
  var f = e.dataTransfer.files
  for (var i=0;i<f.length;i++){
   preview(f[i],d)
   var x=new xhr(d)
   x.open('put',d.put,true)
   x.setRequestHeader('Content-Type','image/png')
   //x.setRequestHeader('Authorization',)
   if(d.ETag){x.setRequestHeader('If-Match',d.ETag)}
   x.send(f[i])
  }
 }
 d.addEventListener('dragenter',dragenter,false)
 d.addEventListener('dragover',dragover,false)
 d.addEventListener('drop',drop,false)
}

dropBoxWS=function(s,d){
 var dragenter=function(e){e.stopPropagation();e.preventDefault()}
 var dragover=function(e){e.stopPropagation();e.preventDefault()}
 var drop=function(e){e.stopPropagation();e.preventDefault()
  var f = e.dataTransfer.files
  for (var i=0;i<f.length;i++){preview(f[i],d);s.send(f[i])}
 }
 d.addEventListener('dragenter',dragenter,false)
 d.addEventListener('dragover',dragover,false)
 d.addEventListener('drop',drop,false)
}

