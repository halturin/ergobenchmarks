package main

import (
	"fmt"

	"github.com/halturin/ergo"
	"github.com/halturin/ergo/etf"
)

type PongGenServer struct {
	ergo.GenServer
	p *ergo.Process
	l []etf.Pid
}

type Pong struct {
	ergo.GenServer
}

type State struct {
	value int
}

func (gs *PongGenServer) Init(p *ergo.Process, args ...interface{}) (state interface{}) {
	gs.p = p
	return nil
}
func (gs *PongGenServer) HandleCast(message etf.Term, state interface{}) (string, interface{}) {
	return "noreply", state
}
func (gs *PongGenServer) HandleCall(from etf.Tuple, message etf.Term, state interface{}) (string, etf.Term, interface{}) {
	switch message {
	case etf.Atom("stop"):
		fmt.Println("stopping server...")
		for pp := range gs.l {
			gs.p.Send(pp, etf.Atom("stop"))
		}
		return "stop", "normal", state

	default:
		n := message.(etf.Tuple)[1]
		list := []etf.Pid{}
		gsp := &Pong{}
		for i := 0; i < n.(int); i++ {
			process, _ := gs.p.Node.Spawn("", ergo.ProcessOptions{}, gsp)
			list = append(list, process.Self())

		}
		fmt.Printf("started %d pong servers\n", n)
		gs.l = list
		return "reply", list, state

	}
}

func (gs *PongGenServer) HandleInfo(message etf.Term, state interface{}) (string, interface{}) {
	fmt.Printf("HandleInfo: %#v  \n", message)
	return "noreply", state
}

func (gs *PongGenServer) Terminate(reason string, state interface{}) {
	fmt.Printf("Terminate: %#v \n", reason)
}

func (gs *Pong) Init(p *ergo.Process, args ...interface{}) (state interface{}) {
	return nil
}
func (gs *Pong) HandleCast(message etf.Term, state interface{}) (string, interface{}) {
	return "stop", state
}
func (gs *Pong) HandleCall(from etf.Tuple, message etf.Term, state interface{}) (string, etf.Term, interface{}) {
	return "reply", etf.Atom("ok"), state
}
func (gs *Pong) HandleInfo(message etf.Term, state interface{}) (string, interface{}) {
	return "noreply", state
}
func (gs *Pong) Terminate(reason string, state interface{}) {
}

func main() {
	node := ergo.CreateNode("erl-demo@127.0.0.1", "cookie123", ergo.NodeOptions{})
	gs1 := &PongGenServer{}

	// spawn new process of genserver
	process, _ := node.Spawn("pong_server", ergo.ProcessOptions{}, gs1)

	// waiting for the process termination.
	process.Wait()
}
