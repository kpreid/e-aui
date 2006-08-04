pragma.enable("easy-return")
pragma.disable("explicit-result-guard")

pragma.enable("accumulator")
pragma.enable("verb-curry")
pragma.enable("easy-when")
pragma.enable("anon-lambda")

def <aui> := <import>[meta.context().getFQNPrefix().split("$")[0] + ".*"]

interface Zero guards ZeroStamp {}
def One := <type:org.erights.e.elib.slot.FinalSlot>
def one(value) { return &value }
interface Many guards ManyStamp {}
def ZOM extends any[Zero, One, Many] {
  to get(valueType) {
    return def ZOM1 {
      to coerce(sp, ej) {
        def zom := super.coerce(sp)
        if (sp == Zero || sp == Many) {
          return sp
        } else {
          return one(valueType.coerce(sp.getValue(), ej))
        }
      }
    }
  }
}
def ZO := any[Zero, One]
def zero implements ZeroStamp {}
def many implements ManyStamp {}

def makeLamportSlot := <import:org.erights.e.elib.slot.makeLamportSlot>
def whenever := <elib:slot.whenever>
def toKey                    := <elib:tables.makeTraversalKey>
def FinalSlot := <type:org.erights.e.elib.slot.FinalSlot>
def Slot := <type:org.erights.e.elib.slot.Slot>
def EverReporter := <type:org.erights.e.elib.slot.EverReporter>
def makeStoneCast := <import:org.erights.e.facet.makeStoneCast>
def File := <type:java.io.File>

interface TypedReporterSlot extends EverReporter {
  # XXX is it appropriate to duplicate the extends' method info here?
  to valueType() :ValueGuard
}

/** make something like a LamportSlot except that it publishes a guard and does not allow itself to be subscribed to a reporter. will not coerce values arriving from upstream. */
def makeTypedReporterSlot(slot, Value) {
  return def typedReporterSlot extends makeStoneCast(slot, EverReporter) {
    to setValue(new) { slot.setValue(Value.coerce(new, null)) }
    to valueType() { return Value }
  }
}

def CompleteCommand
interface Command {
  to snapshot(partialEj) :CompleteCommand
  to "&available"() :Slot
}

interface Presenter {
  to run(ref :any, context) :any
}

interface bind CompleteCommand extends Command {
  to run() :Tuple[any, nullOk[Presenter]]
}

def CommandMessageDesc := <type:org.erights.e.elib.base.MessageDesc> # XXX should be just DeepFrozen and an interface

def makeZeroSlots(params) { 
  return accum [] for pd in params { 
    _.with(makeTypedReporterSlot(makeLamportSlot(zero), ZOM[if (pd.getOptGuard() != null) { pd.getOptGuard() } else { any }]))
  }
}

interface SimpleMessageCommand guards SimpleMessageCommandStamp {}
def makeSimpleMessageCommand(recipient :any, verb :any, args :List[any]) {
  return def simpleMessageCommand implements Command, CompleteCommand, SimpleMessageCommandStamp {
    to getRecipient() { return recipient }
    to getVerb() { return verb }
    to getArgs() { return args }
    to "&available"() { return &true }
    to snapshot() { return simpleMessageCommand }
    to run() { return [E.send(recipient, verb, args), null] }
  }
}

interface FlexArgMessageCommand guards FlexArgMessageCommandStamp {}
def makeFlexArgMessageCommand {
  to run(recipient :any, messageDesc :CommandMessageDesc, argZoSlots :List) {
    require(messageDesc.getParams().size() == argZoSlots.size())
    
    def &available := whenever(argZoSlots, thunk {
      escape e {
        for slot in argZoSlots { if (slot.getValue() !~ o :One) { e(false) } }
        true
      }
    })
    
    return def flexArgMessageCommand implements Command, FlexArgMessageCommandStamp {
      to getRecipient() { return recipient }
      to getVerb() { return messageDesc.getVerb() }
      to getParams() { return messageDesc.getParams() }
      to getArgZoSlots() { return argZoSlots }
      to run() {
        return flexArgMessageCommand.snapshot(null).run()
      }
      to "&available"() { return &available }
      to snapshot(partialEj) {
        return makeSimpleMessageCommand(recipient, messageDesc.getVerb(), accum [] for s in argZoSlots { _.with(
          switch (s.getValue()) {
            match one :One { one.getValue() }
            match _        { throw.eject(partialEj, "incomplete command") }
          }) })
      }
    }
  }
  to empty(r, d :CommandMessageDesc) {
    return makeFlexArgMessageCommand(r, d, makeZeroSlots(d.getParams()))
  }
}

def makeArglessMessageCommand(recipient :any, messageDesc :CommandMessageDesc) {
  return if (messageDesc.getParams().size() > 0) {
    makeSimpleMessageCommand(makeFlexArgMessageCommand, "empty", [recipient, messageDesc])
  } else {
    makeSimpleMessageCommand(recipient, messageDesc.getVerb(), [])
  }
}

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
  #def oisw := makeArglessMessageCommand(openInSeparateWindow, openInSeparateWindow.__getAllegedType().getMessageTypes()["run/1"])
 
  return when (object <- __getAllegedType()) -> _(allegedType) {
    return accum [] for desc ? (desc.getVerb().indexOf1("()"[0]) == -1) in allegedType.getMessageTypes() {
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

def textLimit := 60

def presentGenericInSwing(object, context) {
  def quoted := context.quoting()

  def label := context.kit().plabel("", null, context, object, thunk {thunk {[null,null]}})

  def update() {
    def t := if (quoted) {E.toQuote(object)} else {E.toString(object)}
    label.setText(if (t.size() > textLimit) { t(0, textLimit - 3) + "..." } else {t})
  }
  
  if (!Ref.isResolved(object)) {
    Ref.whenResolved(object, def done(_) :void {
      update()
    })
  }
  update()

  label.addMouseListener(def mouseOverUpdateListener {
    to mouseEntered(event) :void {
      try {
        update()
      } catch problem {
        throw <- (problem) # reach tracelog
      }
    }
    match _ {}
  })
  
  return label
}

def presentCompleteCommandInSwing(command, context) {
  def hole := JPanel``
  hole.setLayout(<awt:FlowLayout>())
  #hole.setPreferredSize(<awt:Dimension>(320, 100))

  def runButton := context.kit().button("Run", thunk {
    #hole.setPreferredSize(null)
    hole."add(Component)"(context.subPresent(command.run(), true))
    hole.revalidate()
  })

  return JPanel`
    ${context.kit().x(JPanel``, runButton)}.X
    $hole.X.Y
  `
}

def setupComponentReactor := <aui:swing.setupComponentReactor>
def makeCommandUI(command :Command, editUI, context) {
  def hole := JPanel``
  hole.setLayout(<awt:FlowLayout>())
  hole.setPreferredSize(<awt:Dimension>(320, 100))

  def runButton := context.kit().button("Run", thunk {
    hole.setPreferredSize(null)
    hole."add(Component)"(context.subPresent(command.run(), true))
    hole.revalidate()
  })
  
  # XXX support far commands?
  def &available := command."&available"()
  
  def updateAvailable(available) {
    runButton.setEnabled(available)
  }
  
  if ((&available).__respondsTo("whenUpdated", 1)) {
    stderr.println(`lamport branch for ${&available}`)
    runButton.setEnabled(false)
    setupComponentReactor(runButton, &available, updateAvailable, null)
  } else {
    stderr.println(`static branch for ${&available}`)
    updateAvailable(available)
  }

  return JPanel`
    $editUI.X
    ${context.kit().x(JPanel``, runButton)}.X
    $hole.Y
  `
}

bind presentFAMCommandInSwing(command, context) {
  def descs := command.getParams()

  def selsC := <swing:Box>(1)
  for i => zos in command.getArgZoSlots() { 
    selsC."add(Component)"(context.kit().x(
      context.subPresent(descs[i], false),
      context.subPresentType(zos, context.kit().getObjectSelectorPresenter(), true),
    )) 
  }

  def leftLabel := JPanel`${context.subPresent(command.getRecipient(), true)} ${context.kit().text(" <- ")} ${context.subPresent(command.getVerb(), false)} ${context.kit().text("(")}`

  return makeCommandUI(command, JPanel`
    $leftLabel $selsC ${context.kit().text(")")}
  `, context)
}

/** demo thing */
def presentMakeFlexMapCommandInSwing(command, context) {
  
  def [kzs, vzs] := command.getArgZoSlots()
  def ksel := context.subPresentType(kzs, context.kit().getObjectSelectorPresenter(), false)
  def vsel := context.subPresentType(vzs, context.kit().getObjectSelectorPresenter(), false)

  return makeCommandUI(command, JPanel`
    ${context.kit().text("Key type: ")}   $ksel.X
    ${context.kit().text("Value type: ")} $vsel.X
  `, context)
}

def presentListY(list, context) {
  def box := E.call(context.kit(), "y", accum [] for item in list { _.with(context.subPresent(item, context.quoting())) })
  return box
}
def makeGridBagTable := <aui:swing.makeGridBagTable>
def presentMapY(map, context) {
  def table := makeGridBagTable()
  def tableC := table.getComponent()
  def gbc := <awt:GridBagConstraints>
  
  var i := 0
  for key => value in map { 
    #println(`$key $value`)
    def keyC := context.subPresent(key, context.quoting())
    def valueC := context.subPresent(value, context.quoting())
    tableC.add(
        keyC,
        gbc(
          0, i, # x/y
          1, 1,   # width/height
          1, 1,   # weight
          gbc.getEAST(), # anchor
          gbc.getVERTICAL(),   # fill
          <awt:Insets>(0, 0, 0, 0), # insets
          0, 0    # ipad
        )
      )
    tableC.add(
        valueC,
        gbc(
          1, i, # x/y
          1, 1,   # width/height
          1, 1,   # weight
          gbc.getWEST(), # anchor
          gbc.getBOTH(),   # fill
          <awt:Insets>(0, 0, 0, 0), # insets
          0, 0    # ipad
        )
      )
    table.addRow(key, [keyC, valueC]) 
    i += 1
  }
  return tableC
}

def presentSwitching(promise, context) {
  def placeholder := presentGenericInSwing(promise, context)

  def hole := context.kit().x(placeholder)
  
  Ref.whenResolved(promise, def switchPresentation(_) {
    #println(`resolved $promise`)
    hole.remove(placeholder)
    #println(`removed placeholder`)
    hole."add(Component)"(presentInSwing(promise, context))
    #println(`replaced to $sp for $promise, components are`)
    #println(hole.getComponents())
    hole.revalidate()
  })
  
  return hole
}

bind presentInSwing(object, context) {
  return switch (object) {
    match p ? (!Ref.isResolved(p)) {
      presentSwitching(p, context) }
    match x :List ? (x !~ y :String) {
      presentListY(x, context) }
    match x :Map {
      presentMapY(x, context) }
    match x :FinalSlot {
      context.subPresent(x.getValue(), context.quoting()) }

    #match x :near ? x.__respondsTo("paintIcon", 4) { # XXX better solution needed -- Java interfaces should respond to something like actuallyDeclared to give a restrictive guard
    #  context.kit().text(x) }
      
    # broken - not all lamports hold ZOMs
    #match ls :near ? (ls.__getAllegedType().getFQName() =~ `org.erights.e.elib.slot.makeLamportSlot$$makeLamportSlot__C$$lamportSlot__@{_}__C`) { 
    #  context.kit().getObjectSelectorPresenter()(ls, context) }
    match c :FlexArgMessageCommand ? ([c.getRecipient(), c.getVerb(), c.getArgZoSlots().size()] == [<elib:tables.makeFlexMap>, "fromTypes", 2]) { 
      presentMakeFlexMapCommandInSwing(object, context) }

    match c :FlexArgMessageCommand { 
      presentFAMCommandInSwing(object, context) }
    match c :SimpleMessageCommand { 
      presentCompleteCommandInSwing(object, context) }

    match _ { 
      presentGenericInSwing(object, context) }
  }
}

bind runToWindow(title, command, originC) {
  def context := backend.getRootContext()
  def [result, optPresenter] := command.run()
  backend.openFrame(title, 
                 if (optPresenter != null) \
                   {context.subPresentType(result, optPresenter, true)} else \
                   {context.subPresent(result, true)}, 
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
  def container := context.kit().y()
  for name => file in dir {
    container."add(Component)"(context.subPresentType(file, presentDirEntry(name), true))
  }
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
