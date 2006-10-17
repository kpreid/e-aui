# Copyright 2006 Kevin Reid, under the terms of the MIT X license
# found at http://www.opensource.org/licenses/mit-license.html ................

pragma.enable("easy-return")
pragma.disable("explicit-result-guard")

pragma.enable("accumulator")
pragma.enable("verb-curry")
pragma.enable("easy-when")
pragma.enable("anon-lambda")

def <aui> := <import:org.cubik.cle.aui.*>

def makeLamportSlot := <import:org.erights.e.elib.slot.makeLamportSlot>

def Command                  := <aui:command.Command>
def gatherCommands

def [=> SimpleMessageCommand,
     => makeSimpleMessageCommand,
     => makeCompleteCommand,
     => FlexArgCommand,
     => makeFlexArgCommand,
     => makeArglessMessageCommand,
     => CommandMessageDesc,
     => Zero, => One, => Many,
     => zero, => one, => many,
     => ZOM, => ZO] | _ \
  := def auiCommon := <aui:command.messyCommandDefinitions> \
                    | <aui:data.zeroOneMany> \
                    | [=> gatherCommands]


def openInSeparateWindow extends makeSimpleMessageCommand.extract(__identityFunc, "run", 1) implements Command {}

bind gatherCommands(object) :vow[List[Command]] {
  # XXX we need general architecture for collecting 1-arg commands and filling them into the list
 
  def baseCommands := [makeCompleteCommand(openInSeparateWindow, [object])]
 
  return when (object <- __getAllegedType()) -> _(allegedType) {
    return accum baseCommands for desc ? (desc.getVerb().indexOf1("()"[0]) == -1) in allegedType.getMessageTypes() {
      _.with(makeArglessMessageCommand(object, desc))
    }
  }
}

def presentDefault := <aui:present.makeDefaultPresenter>(auiCommon)

# ------------------------------------------------------------------------------

# "backend" name borrowed from McCLIM

if (currentVat.getRunnerKind() != "awt") {
  interp.waitAtTop(currentVat.morphInto("awt"))
}
def backend := <aui:swing.makeSwingBackend>(<awt>, <swing>, presentDefault, <aui:present.makeDefaultIconPresenter>(), auiCommon, stdout)

def rootsFlex := [].asMap().diverge()

# ------------------------------------------------------------------------------

def parse := e__quasiParser

def borderFactory := <swing:makeBorderFactory>
def awtDropTarget := <awt:dnd.makeDropTarget>
def presentCaplet(capletFile) {
  def capletAuthor := parse(capletFile.getTwine()).eval(safeScope)
  def capletName := E.toString(capletFile)
  return def presenter(document, context) {
    
    def container := JPanel``
     
    var takeContainer := thunk { container }
    
    def makeCapletFrame {
      to run() {
        def ffOuter := takeContainer()
        takeContainer := thunk { throw("multiple caplet frames not implemented yet") }

        ffOuter.setBorder(def tb := borderFactory.createTitledBorder(capletName))
        ffOuter."add(Component)"(def userPane := JPanel``)

        def capletFrame {
          to setTitle(title :String) :void {tb.setTitle(`$capletName - $title`); ffOuter.revalidate()}
          #to setState(state :boolean) :void {myJFrame.setState(state)}
          #to setResizable(state :boolean) :void {myJFrame.setResizable(state)}
          #to setJMenuBar(bar :JMenuBar) :void {myJFrame.setJMenuBar(bar)}
          to setDefaultCloseOperation(op :int) :void {stderr.println("XXX need to implement setDefaultCloseOperation")}
          to addWindowListener(lis) :void {stderr.println("XXX need to implement addWindowListener")}
          to removeWindowListener(lis) :void {stderr.println("XXX need to implement removeWindowListener")}
          #to setCursor(cursor :Cursor) :void {myJFrame.setCursor(cursor)}
          to getContentPane() :near {
              return userPane
          }
          #match [verb, []] {
          #    if (validNoArgVerbs.maps(verb)) {
          #        E.call(myJFrame, verb, [])
          #    }
          #}
        }
        return capletFrame
      }
    }
    
    def keys := "INITIAL_DOC_RCVRS TRACELN FRAME_MAKER DROP_TARGET".split(" ").asSet()
    
    def powerbox {
      to optCap(key) {
        switch (key) {
          match =="INITIAL_DOC_RCVRS" {
            return [document]
          }
          match =="TRACELN" {
            return def traceln {
              match [`run`, pieces] { 
                def prefix := `$capletFile: `
                def sub := stderr.indent(" " * prefix.size())
                stderr.print(prefix)
                for piece in pieces {
                  sub.print(piece)
                }
                stderr.println()
                null
              }
            }
          }
          match =="FRAME_MAKER" {
            return makeCapletFrame
          }
          match =="DROP_TARGET" { return awtDropTarget }
        }
      }
      
      to requestFileRcvrsVow(editable :boolean,
                             title :String,
                             justification :String,
                             startPathText :String,
                             optFilter :pbc) {
        # XXX this code is variously Wrong
        # also, it ignores startPathText and optFilter
        def files
        
        def fileSlot := makeLamportSlot(zero)
        
        def fsel := backend.getRootContext().subPresentType(fileSlot, context.kit().getObjectSelectorPresenter(), false)
        
        def run
        def ui := JPanel`
          ${context.kit().text(`Justification: "$justification"`)}
          $fsel >
          ${JPanel``}.X ${context.kit().button("Run", run)}
        `

        def context := backend.getRootContext()
        def frame := backend.openFrame(`CapDesk - $capletName requests ${editable.pick("editable", "read-only")} file: $title`,
                                    ui,
                                    container)
        
        bind run() {
          bind files := [fileSlot.getValue().getValue()]
          frame.hide()
        }
        
        return files
      }

      match [`get@{key ? (keys.contains(key))}`, []] { key }

    }
    
    capletAuthor(powerbox)
    
    return container
  }
}

# ------------------------------------------------------------------------------

# File-browser application example

def openCommand

def presentDirEntry(name) {
  return def present(file, context) {
    return context.kit().plabel(name, null, context, file, thunk { makeCompleteCommand(openCommand, [file]) })
  }
}

def presentDirectory(dir, context) {
  def container := E.call(context.kit(), "y",
    accum [] for name => file in dir {
      _.with(context.subPresentType(file, presentDirEntry(name), true))
    })
  return container
}

bind openCommand implements Command {
  to "&available"([_]) { return &true }
  to run(file) { 
    return if (file.isDirectory()) {
      [file, presentDirectory]
    } else {
      [file, presentCaplet(<file:/Stuff/e/caplets/capEdit/capEdit.caplet>)]
    }
  }
}

def exampleFile := <file:/>

rootsFlex["File Browser"] := thunk { 
  [exampleFile, presentDirectory]
}

# ------------------------------------------------------------------------------

# REPL

def seqEval := { 
  var env := privilegedScope
  def seqEval(expr) {
    def [v, e] := expr.evalToPair(env)
    env := e
    return v
  }
}

def presentSeqEval(seqEval, context) {
  def box := context.kit().y(def filler := JPanel``)
  filler.setPreferredSize(<awt:makeDimension>(320,320))

  box."add(Component)"(context.kit()._textLineCommandField(def handle(line) {
    # XXX needs cleanup.
    def expr := e__quasiParser(line)
    box."add(Component, int)"(
      JPanel`${context.kit().text("? ")} ${context.subPresent(expr, false)} ${JPanel``}.X.Y
             ${JPanel``}.Y             V                                             V
      `, box.getComponentCount() - 1)
    box."add(Component, int)"(context.subPresent(seqEval <- (expr), true), box.getComponentCount() - 1)
    box.revalidate()
  }))

  return box
}

rootsFlex["Repl"] := thunk {
  [seqEval, presentSeqEval]
} 

# ------------------------------------------------------------------------------

def makeClock(timer, resolution) {
  def reduce(t) { return t // resolution * resolution }
  
  def clock {
    to getValue() { return reduce(timer.now()) }

    to whenUpdated(reactor) {
      clock.whenUpdated(reactor, -1)
    }

    to whenUpdated(reactor, knownGen) {
      timer.whenPast(reduce(knownGen) + resolution,
                     def sendClockUpdate() {
        def answer := reduce(timer.now())
        reactor <- reactToUpdate(answer, answer, clock)
      })
    }
  }
  
  return clock
}

# XXX needs a time-value type
rootsFlex["Clock"] := thunk { 
  [makeClock(timer, 1000), null]
}

# ------------------------------------------------------------------------------

rootsFlex["Things"] := thunk {
  def exampleButtonCommand := makeArglessMessageCommand(def tk := <awt:makeToolkit>.getDefaultToolkit(), tk.__getAllegedType().getMessageTypes()["beep/0"])

  [[
    => exampleButtonCommand, 
    => Zero, => One, => Many, 
    => zero, => one, => many, 
    => ZOM, => ZO, 
    => backend, 
    => privilegedScope, 
    => safeScope,
  ], null]
}

rootsFlex["Expression"] := thunk { 
  def expr := e`
    pragma.enable("easy-return")
    pragma.disable("explicit-result-guard")
    def object extends 1.0 implements Example {
      to makeCounter() {
        return { var x := 0; [thunk { x += 1 }, &x] }
      }
      to "exit"() {
        try { throw("biff") } catch p { throw(p) } finally { notice('q') }
      }
      to patterns() {
        def [_, a, var b, &c, d] + g := data # , via (e) f
      }
      to "meta"() {
        [meta.getState(),
         meta.context()]
      }
      to plumbing() { def drain match water { flush(water) } }
    }
  `
  [expr, null]
}

# ------------------------------------------------------------------------------

def roots := rootsFlex.snapshot()

def presentNamedRootMaker(name) {
  def invokeCommand implements Command {
    to "&available"([_]) { return &true }
    to run(f) { 
      return f()
    }
  }

  return def namePresent(object, context) {
    return context.kit().plabel(name, null, context, object, thunk { makeCompleteCommand(invokeCommand, [object]) })
  }
}

def presentRoots(map, context) {
  def container := E.call(context.kit(), "y",
    accum [] for name => maker in map {
      _.with(context.subPresentType(maker, presentNamedRootMaker(name), true))
    })
  return container
}

backend.openFrame("Roots", backend.getRootContext().subPresentType(roots, presentRoots, false), null) 
