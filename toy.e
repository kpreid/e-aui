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
def CompleteCommand          := <aui:command.CompleteCommand>
def gatherCommands

def [=> SimpleMessageCommand,
     => makeSimpleMessageCommand,
     => FlexArgMessageCommand,
     => makeFlexArgMessageCommand,
     => makeArglessMessageCommand,
     => CommandMessageDesc,
     => Zero, => One, => Many,
     => zero, => one, => many,
     => ZOM, => ZO] | _ \
  := def auiCommon := <aui:command.messyCommandDefinitions> \
                    | <aui:data.zeroOneMany> \
                    | [=> gatherCommands]


bind gatherCommands(object) :vow[List[Command]] {
  # XXX we need general architecture for command templates filled in with a single object, and presenting them

  # XXX clean this up
  def openInSeparateWindow extends makeSimpleMessageCommand(__identityFunc, "run", [object]) implements Command, CompleteCommand {}
 
  def baseCommands := [openInSeparateWindow]
 
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
def backend := <aui:swing.makeSwingBackend>(<awt>, <swing>, presentDefault, <aui:present.makeDefaultIconPresenter>(), auiCommon)

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

def makeOpenCommand

def presentDirEntry(name) {
  return def present(file, context) {
    return context.kit().plabel(name, null, context, file, thunk { makeOpenCommand(file) })
  }
}

def presentDirectory(dir, context) {
  def container := E.call(context.kit(), "y",
    accum [] for name => file in dir {
      _.with(context.subPresentType(file, presentDirEntry(name), true))
    })
  return container
}

interface OpenCommand guards OpenCommandStamp {}
bind makeOpenCommand(file) {
  return def openCommand implements Command, CompleteCommand, OpenCommandStamp {
    to "&available"() { return &true }
    to snapshot() { return openCommand }
    to run() { 
      return if (file.isDirectory()) {
        [file, presentDirectory]
      } else {
        [file, presentCaplet(<file:/Stuff/e/caplets/capEdit/capEdit.caplet>)]
      }
    }
  }
}

def exampleFile := <file:/Stuff>

backend.openFrame("File Browser", backend.getRootContext().subPresentType(exampleFile, presentDirectory, true), null) 

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

backend.openFrame("Repl", backend.getRootContext().subPresentType(seqEval, presentSeqEval, false), null) 

# ------------------------------------------------------------------------------

#def cmd := makeFlexArgMessageCommand(def x := <elib:tables.makeFlexMap>, x.__getAllegedType().getMessageTypes()["fromTypes/2"], [makeLamportSlot(one(<type:java.lang.Object>)), makeLamportSlot(one(<type:java.lang.Object>))])

def exampleButtonCommand := makeArglessMessageCommand(def tk := <awt:makeToolkit>.getDefaultToolkit(), tk.__getAllegedType().getMessageTypes()["beep/0"])

# meta.getState() 
# cmd 
# [1, 2, 3, [example, 4, 5, 6]]
def example := [
  => exampleButtonCommand, 
  => Zero, => One, => Many, 
  => zero, => one, => many, 
  => ZOM, => ZO, 
  => backend, 
  => privilegedScope, 
  => safeScope,
]

# disabled for now as the REPL incorporates this mostly
backend.openFrame("Toy", backend.getRootContext().subPresent(example, true), null) 
