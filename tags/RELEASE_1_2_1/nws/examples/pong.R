src = function(...) { source('nws.R') }
tryCatch(library(nws), error=src)
host = 'localhost'
port = 8765
wsname = 'ping-pong'
loops = 10

nws = new('netWorkSpace', wsname, host, port)
game = nwsFetch(nws, 'game')
nwsStore(nws, 'game', game + 1)
pong = paste('pong_', game, sep="")

cat('Starting a ping-pong game ', game, '\n')
for (i in 1:loops) {
    nwsStore(nws, 'ping', pong)
    reply = nwsFetch(nws, pong)
    cat(reply, '\n')
}

nwsDeleteVar(nws, pong)
