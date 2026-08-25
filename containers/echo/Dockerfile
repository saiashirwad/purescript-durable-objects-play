# A tiny HTTP echo server: answers every request with what it saw.
FROM node:22-alpine
EXPOSE 8080
CMD ["node", "-e", "process.on('SIGTERM',()=>process.exit(0));let hits=0;require('http').createServer((q,s)=>{let b='';q.on('data',c=>b+=c);q.on('end',()=>{s.setHeader('content-type','application/json');s.end(JSON.stringify({greeting:process.env.GREETING||null,method:q.method,path:q.url,body:b,hits:++hits}))})}).listen(8080)"]
