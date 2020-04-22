// ergo -> erlang

// run erlang
// erl -name erl-demo@127.0.0.1 -setcookie cookie123
// (erl-demo@127.0.0.1)1> pong:start_pong().
//
// run benchmark
// go test -bench=. -benchmem -benchtime=10s -message=pid [string,pid,list]

package main

import (
	"flag"
	"fmt"
	"github.com/halturin/ergo"
	"github.com/halturin/ergo/etf"
	"runtime"
	"testing"
)

var message string
var msg interface{}

func init() {
	flag.StringVar(&message, "message", "", "number(default), string, pid, list")
}

type benchGS struct {
	ergo.GenServer
}

func (b *benchGS) Init(p *ergo.Process, args ...interface{}) interface{} {
	return nil
}

func (b *benchGS) HandleCall(from etf.Tuple, message etf.Term, state interface{}) (string, etf.Term, interface{}) {
	return "reply", etf.Atom("ok"), state
}

func (b *benchGS) HandleCast(message etf.Term, state interface{}) (string, interface{}) {
	return "noreply", state
}

func (b *benchGS) HandleInfo(message etf.Term, state interface{}) (string, interface{}) {
	return "noreply", state
}

func (b *benchGS) Terminate(reason string, state interface{}) {

}

func BenchmarkNodeParallel(b *testing.B) {

	node1name := fmt.Sprintf("nodeB1Parallel_%d@localhost", b.N)
	node1 := ergo.CreateNode(node1name, "cookie123", ergo.NodeOptions{})
	p2 := etf.Tuple{etf.Atom("pong_server"), etf.Atom("erl-demo@127.0.0.1")}

	p1, _ := node1.Spawn("", ergo.ProcessOptions{}, &benchGS{})
	b.SetParallelism(15)

	ipids, e := p1.Call(p2, etf.Tuple{etf.Atom("start"), 15 * runtime.GOMAXPROCS(-1)})
	if e != nil {
		b.Fatal("single ping", e)
	}
	pids := ipids.(etf.List)
	ii := 0

	switch message {
	case "tuple":
		msg = etf.Tuple{
			etf.Atom("key1"), 1234567890, "key2", 1.23456789, 1234567890, "value1",
			1.23456789, etf.Atom("value2"), etf.Atom("pid1"), p1.Self(),
		}
	case "map":
		msg = etf.Map{
			etf.Atom("key1"): 1234567890,
			"key2":           1.23456789,
			1234567890:       "value1",
			1.23456789:       etf.Atom("value2"),
			etf.Atom("pid1"): p1.Self(),
			etf.Atom("key2"): 1234567890,
			"key3":           1.23456789,
			1234567891:       "value2",
			1.23456790:       etf.Atom("value3"),
			etf.Atom("pid2"): p1.Self(),
		}
	case "list":
		list := []etf.Pid{}
		for i := 0; i < 200; i++ {
			list = append(list, p1.Self())
		}
		msg = list
	default:
		msg = 1234567890
	}
	b.RunParallel(func(pb *testing.PB) {
		p1, e1 := node1.Spawn("", ergo.ProcessOptions{}, &benchGS{})
		if e1 != nil {
			b.Fatal(e1)
		}
		pp := pids[ii]
		ii++
		b.ResetTimer()
		for pb.Next() {
			_, e := p1.Call(pp, etf.Atom("ping"))
			if e != nil {
				b.Fatal(e)
			}
		}

	})
}
