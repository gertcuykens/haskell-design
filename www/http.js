createWebSocket=function(path) {
 var host = window.location.hostname;
 if(host == '') host = 'localhost';
 var uri = 'ws://' + host + path;
 var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
 return new Socket(uri);
}

dropBoxWS=function(s,d){
 var dragenter=function(e){e.stopPropagation();e.preventDefault()}
 var dragover=function(e){e.stopPropagation();e.preventDefault()}
 var drop=function(e){e.stopPropagation();e.preventDefault()
  var f = e.dataTransfer.files
  for (var i=0;i<f.length;i++){preview(f[i],d);s.send(f[i])}
  //console.log(s.bufferedAmount)
 }
 var click=function(e){input.click()}
 var change=function(e){for (var i=0;i<this.files.length;i++){preview(this.files[i],d);s.send(this.files[i])}}
 var input=document.createElement('input')
 input.type='file'
 input.addEventListener('change',change,false)
 d.addEventListener('dragenter',dragenter,false)
 d.addEventListener('dragover',dragover,false)
 d.addEventListener('drop',drop,false)
 d.addEventListener('click',click,false)
}

preview=function(f,d){
 var reader=new FileReader()
 reader.onload=(function(i){return function(e){i.src=e.target.result}})(d)
 reader.readAsDataURL(f)
 //console.log(f.name)
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

function setCookie(k,v,x)
{
 var t=new Date();
 t.setDate(t.getDate() + x);
 var v=escape(v) + ((x==null) ? "" : "; expires="+t.toUTCString());
 document.cookie=k + "=" + v;
}

function getCookie(a)
{
 var i,k,v,c=document.cookie.split(";");
 for (i=0;i<c.length;i++)
 {
  k=c[i].substr(0,c[i].indexOf("="));
  k=k.replace(/^\s+|\s+$/g,"");
  v=c[i].substr(c[i].indexOf("=")+1);
  if (k==a) return unescape(v);
 }
}

/*
reader.addEventListener('load',(function(i){return function(e){i.src=e.target.result}})(d),false)

progress = function(s){
 var progress=document.createElement('progress')
 progress.value=0
 progress.max=100
 progress.style.backgroundColor='green'
 progress.addEventListener('click',function(e){s.abort()},false)
 document.body.appendChild(progress)
 console.log(s.bufferedAmount)
}

xhr=function(o){
 var x=new XMLHttpRequest()
 x.addEventListener('loadstart',function(e){progress.style.display='inline-block';progress.style.backgroundColor='green';progress.value=0},false)
 x.addEventListener('abort',function(e){progress.style.display='none'},false)
 x.addEventListener('progress',function(e){if(e.lengthComputable)progress.value=Math.round((e.loaded*100)/e.total)},false)
 x.addEventListener('error',function(e){progress.style.backgroundColor='red'},false)
 x.addEventListener('load',function(e){progress.style.display='none';if(this.getResponseHeader('ETag')){o.ETag=this.getResponseHeader('ETag');return 0};if(this.getResponseHeader('X-Couch-Update-NewRev')){o.ETag=this.getResponseHeader('X-Couch-Update-NewRev');return 0}},false)
 progress(x)
 return x
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
*/

