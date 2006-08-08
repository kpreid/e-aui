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
def toKey                    := <elib:tables.makeTraversalKey>
def File := <type:java.io.File>

def Command                  := <aui:command.Command>
def CompleteCommand          := <aui:command.CompleteCommand>

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
                    | <aui:data.zeroOneMany>

def makePresentationContext(makePresentationBoundary, present, seen, quoting, hooks, kit) {
  return def presentationContext {
    to quoting() { return quoting }
    to kit() { return kit }
    to getHooks() { return hooks } # XXX in the ideal system this should not exist
    
    to subPresent(object, quoting) {
      return presentationContext.subPresentType(object, present, quoting)
    }
    
    to subPresentType(object, type, quoting) {
      def key := toKey(object)
      def hooks
      def subC := makePresentationContext(makePresentationBoundary, present, seen.with(key), quoting, hooks, kit)
      def [c, bind hooks] := makePresentationBoundary(object, subC, if (seen.contains(key)) {
        present("CYCLE", subC)
      } else {
        type(object, subC)
      })
      return c
    }
  }
}

def gatherCommands(object) :vow[List[Command]] {
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

# ------------------------------------------------------------------------------

# "backend" name borrowed from McCLIM

def presentInSwing
def presentAsIcon
def makeIconContext

def presentFAMCommandInSwing
def presentAMCommand
def runToWindow

def makeSwingBackend() {
  def action := <aui:swing.action>
  def awtDropTarget := <awt:dnd.makeDropTarget>
  def borderFactory := <swing:makeBorderFactory>
  def makeJLabel := <swing:makeJLabel>
  def makeImageIcon := <swing:makeImageIcon>
  def makeObjectSelector := <aui:swing.makeObjectSelectorAuthor>(one, zero, awtDropTarget)
  def swingConstants := <swing:makeSwingConstants>
    def dragDropKit := <import:com.skyhunter.e.awt.dnd.dragDropKit>(<awt>, def _(_) {})
  def attachContextMenu := <aui:swing.attachContextMenu>
  def VK_ENTER := <awt:event.KeyEvent>.getVK_ENTER()
  def makeJTextField := <swing:makeJTextField>
  
  def menuPresentKit {
    to button(name :String, actionThunk) {
      def component := <swing:makeJMenuItem>(name)
      action(component, actionThunk)
      return component
    }
  }
  
  def iconPresentKit {
    to image(resource) {
      return makeImageIcon(resource)
    }
  }
  
  def addStandardEventHandlers(component, object, outerContext) {
    dragDropKit.setupLocalDragSource(component, thunk { object })
    outerContext <- getHooks() <- get("addListeners") <- (component) # XXX this should not be deferred, but we have a cyclic dependency
  
    attachContextMenu(component, thunk {
      def m := <swing:JPopupMenu>()
      def menuContext := makePresentationContext(fn _,_,c {[c,[].asMap()]}, Ref.broken("no generic present"), [].asSet(), false, [].asMap(), menuPresentKit)
      
      when (gatherCommands(object)) -> doCommandMenu(commands) {
        for command in commands {
          m."add(Component)"(presentAMCommand(command, menuContext, object))
        }
  
        m.addPopupMenuListener(outerContext.getHooks()["borderControlListener"])
  
        return m
      } catch p {
        <awt:Toolkit>.getDefaultToolkit().beep() # besides the tracelog
        throw <- (p)
        return m
      }
    })
  }
  
  def presentKit {
    /** XXX what does this name mean? */
    to plabel(name :String, optIconPresenter, context, object, getDoubleClickCommand) {
    
      def iconContext := makePresentationContext(fn _,_,c {[c,[].asMap()]}, presentAsIcon, [].asSet(), false, [].asMap(), iconPresentKit)
      def icon := if (optIconPresenter != null) {
        iconContext.subPresentType(object, optIconPresenter, false)
      } else { 
        iconContext.subPresent(object, false)
      }
    
      # XXX object arg shouldn't be present; instead the context should be asked to install these things
      def label := makeJLabel(name, icon, swingConstants.getLEADING())
      
      addStandardEventHandlers(label, object, context)
      
      label.addMouseListener(def doubleClickListener {
        to mouseReleased(e) { try {
          if (e.getClickCount() == 2) {
            def command := getDoubleClickCommand()
            runToWindow(E.toString(command), command, label)
          }
        } catch p { throw <- (p) } }
        match _ {}
      })
      
      return label
    }
    
    to button(name :String, actionThunk) {
      def component := <swing:makeJButton>(name)
      action(component, actionThunk)
      return component
    }

    /** XXX this ought to not exist here */
    to getObjectSelectorPresenter() { return makeObjectSelector }

    to text(s :String) {
      return makeJLabel(s)
    }

    to _textLineCommandField(handler) {
      def field := makeJTextField()
      field.addKeyListener(def enterKeyListener {
        to keyPressed(e) :void { try {
          if (e.getKeyCode() == VK_ENTER) {
            handler <- (field.getText())
            field.setText("")
          }
        } catch p { throw <- (p) } }
        match _ {}
      })
      return field
    }

    #match [dn ? via (["x" => 0, "y" => 1].fetch) dir, components]
    match [dn ? (["x" => 0, "y" => 1] =~ [(dn) => dir]|_), components] {
      def box := <swing:makeBox>(dir)
      for c in components { 
        box."add(Component)"(c) 
      }
      box
    }
  }

  def normalBorder := borderFactory.createEmptyBorder(2, 2, 2, 2)
  def boundaryBorder := borderFactory.createRaisedBevelBorder()
  def activeBorder := borderFactory.createBevelBorder(
    <swing:border.BevelBorder>.getRAISED(),
    <awt:Color>."run(float, float, float)"(0.7, 0.7, 1.0),
    <awt:Color>."run(float, float, float)"(0.4, 0.4, 0.7))

  def makeSwingBoundary(object, context, content) {
    def container := JPanel`$content`
  
    addStandardEventHandlers(container, object, context)
  
  
    var mouseover := [].asSet()
    var active := false
  
    def updateBorder() {
      container.setBorder(
        if (active) { activeBorder } else if (mouseover.size() > 0) { boundaryBorder } else { normalBorder } )
    }
    updateBorder()
  
    
    
    def addListeners(c) {
      c.addMouseListener(def mouseOverUpdateListener {
      to mouseEntered(event) :void { try {
        mouseover with= mouseOverUpdateListener
        updateBorder()
      } catch p { throw <- (p) } }
      to mouseExited(event) :void { try {
        mouseover without= mouseOverUpdateListener
        updateBorder()
      } catch p { throw <- (p) } }
      match _ {}
    })
    }
    addListeners(container)
  
    def borderControlListener { # XXX this needs renaming now
      to popupMenuWillBecomeInvisible(e) { try { 
        active := false
        updateBorder()
      } catch p { throw <- (p) } }
      to popupMenuWillBecomeVisible(e) { try {
        active := true
        updateBorder()
      } catch p { throw <- (p) } }
      match msg {}
    }
  
    return [container, [=> borderControlListener, => addListeners]]
  }

  def rootContext := makePresentationContext(makeSwingBoundary, presentInSwing, [].asSet(), true, Ref.broken("no hooks"), presentKit)

  return def backend {
    to getRootContext() { return rootContext }
    to getPresentKit() { return presentKit }
    /** XXX decide whether "Frame" name is to stay */
    to openFrame(title, component, optRel) {
      def frame := <swing:makeJFrame>(title)
    
      def sp := <swing:JScrollPane>()
        #sp.setPreferredSize(<awt:Dimension>(320,320))
        #sp.setVerticalScrollBarPolicy(<swing:ScrollPaneConstants>.getVERTICAL_SCROLLBAR_ALWAYS())
      sp.getViewport()."add(Component)"(component)
    
      frame.setContentPane(sp)
      frame.pack()
      if (optRel != null) { 
        frame.setLocationRelativeTo(optRel)
      } else {
        if (frame.__respondsTo("setLocationByPlatform", 1)) {
          frame.setLocationByPlatform(true)
        }
      }
      frame.show()
      return frame
    }
  }
}

if (currentVat.getRunnerKind() != "awt") {
  interp.waitAtTop(currentVat.morphInto("awt"))
}
def backend := makeSwingBackend()

# ------------------------------------------------------------------------------

def presentCommandRun := <aui:present.presentCommandRun>
bind presentInSwing := <aui:present.makeDefaultPresenter>(auiCommon)

bind runToWindow(title, command, originC) {
  backend.openFrame(title, 
                    presentCommandRun(backend.getRootContext(), 
                                      command.run()),
                    originC)
}

bind presentAMCommand(command :CompleteCommand, context, selected) {
  def [commandLabel, resultLabelTh] := switch (command) {
    match s :SimpleMessageCommand ? (s.getRecipient() == selected) {
      def verb := command.getVerb()
      def arity := command.getArgs().size()
      def more := false
      [verb,
       thunk { `$selected.$verb()` }]
    }
    match s :SimpleMessageCommand ? (s.getRecipient() == makeFlexArgMessageCommand && s.getVerb() == "empty" && s.getArgs() =~ [==selected, md :CommandMessageDesc]) {
      def verb := md.getVerb()
      def arity := md.getParams().size()
      [verb + "." * (2 + arity),
       thunk { `$selected.$verb/$arity` }]
    }
    match _ {
      [def s := E.toQuote(command), thunk { s }]
    }
  }

  def mi := context.kit().button(commandLabel, thunk {
    runToWindow(resultLabelTh(), command, mi)
  })

  return mi
}


# ------------------------------------------------------------------------------

# Icons

# XXX TODO: reestablish caching of icons. how can this be made compatible with context-specific presentation?
bind presentAsIcon(object, context) {
  def kit := context.kit()
  return switch (object) {
    match f :File {
      f.isDirectory().pick(kit.image(<resource:com/skyhunter/capDesk/icons/folder.gif>), 
                           kit.image(<resource:com/skyhunter/capDesk/icons/noLauncher.gif>))
    }
    match _ { kit.image(<resource:org/cubik/cle/aui/swing/item.gif>) }
  }
}

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
  filler.setPreferredSize(<awt:Dimension>(320,320))

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

def exampleButtonCommand := makeArglessMessageCommand(def tk := <awt:Toolkit>.getDefaultToolkit(), tk.__getAllegedType().getMessageTypes()["beep/0"])

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
# backend.openFrame("Toy", rootContext.subPresent(example, true), null) 
