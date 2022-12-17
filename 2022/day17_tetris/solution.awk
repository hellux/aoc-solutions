BEGIN { W=7 }

function set_sprite(n) {
    delete s
    if (n==0) {
        s[0,0]=1; s[1,0]=1; s[2,0]=1; s[3,0]=1
    } else if (n==1) {
                  s[1,2]=1
        s[0,1]=1; s[1,1]=1; s[2,1]=1
                  s[1,0]=1
    } else if (n==2) {
                            s[2,2]=1
                            s[2,1]=1
        s[0,0]=1; s[1,0]=1; s[2,0]=1
    } else if (n==3) {
        s[0,3]=1
        s[0,2]=1
        s[0,1]=1
        s[0,0]=1
    } else {
        s[0,1]=1; s[1,1]=1
        s[0,0]=1; s[1,0]=1
    }
}

function height() {
    m=0
    for (i in rocks) {
        split(i, ps, SUBSEP); yy=ps[2]
        if (rocks[i] && yy+1>m) m=yy+1
    }
    return m
}

function spawn() {
    set_sprite(i_sprite)
    i_sprite=(i_sprite+1)%5
    x=2; y=height()+3
}

function land() {
    rock_count+=1
    for (i in s) {
        if (!s[i]) continue
        split(i, ps, SUBSEP)
        sx=x+ps[1]; sy=y+ps[2]
        rocks[sx,sy]=1
    }
}

function can_move(dx, dy) {
    for (i in s) {
        if (!s[i]) continue
        split(i, ps, SUBSEP)
        nx=x+dx+ps[1]; ny=y+dy+ps[2]
        if (nx<0 || nx>=W || ny<0 || rocks[nx,ny]) return 0
    }
    return 1
}

function push(d) {
    if (d == "<") dx=-1
    else dx=+1
    if (can_move(dx, 0)) x+=dx
}

function fall() {
    if (can_move(0, -1)) {
        y-=1
        return 1
    } else {
        land()
        return 0
    }
}

function draw() {
    for (yy=height()+3; yy>=0; yy--) {
        printf "%01d|", yy
        for (xx=0; xx<W; xx++) {
            if (rocks[xx,yy]) {
                printf "#"
            } else if (s[xx-x,yy-y]) {
                printf "@"
            } else {
                printf "."
            }
        }
        print "|"
    }
    print " +-------+"
    print "  0123456"
}

/./ {
    spawn()
    l=length($0)
    while (rock_count < 2022) {
        d=substr($0, jj+1, 1)
        jj=(jj+1)%l
        #draw()
        push(d)
        #draw()
        fell=fall()
        #draw()
        if (!fell) spawn()
    }
    print height()
}
