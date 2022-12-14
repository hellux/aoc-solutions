BEGIN {
    FS=" -> "
    SX=500; SY=0
}

function min(a, b) { if (a < b) return a; else return b }
function max(a, b) { if (a > b) return a; else return b }

{
    px=0; py=0
    for (f=1; f<=NF; f++) {
        l=split($f, xy, ",")
        wx=xy[1]; wy=xy[2]
        if (px||py) {
            if (px==wx) for (y=min(wy, py); y<=max(wy, py); y++) w[wx,y]=1
            else        for (x=min(wx, px); x<=max(wx, px); x++) w[x,wy]=1
        }
        wmax=max(wy, wmax)
        px=wx; py=wy
    }
}

function fall() {
    n=0
    while (1) {
        n+=1
        x=SX; y=SY
        while (1) {
            if (!w[x,y+1]) {
                y+=1
            } else if (w[x,y+1]) {
                if (!w[x-1,y+1]) {
                    x-=1; y+=1 # left
                } else if (!w[x+1,y+1]) {
                    x+=1; y+=1 # right
                } else {
                    w[x,y]=2
                    break
                }
            }
            if (y>=y_abyss) return n
        }
    }
}

END {
    y_abyss=wmax
    print fall()-1
}
