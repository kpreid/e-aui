pragma.enable("easy-return")
pragma.disable("explicit-result-guard")

pragma.enable("accumulator")
pragma.enable("verb-curry")
pragma.enable("easy-when")

def <aui> := <import>[meta.context().getFQNPrefix().split("$")[0] + ".*"]

interface Zero guards ZeroStamp {}
def One := <type:org.erights.e.elib.slot.FinalSlot>
interface Many guards ManyStamp {}
def ZOM := any[Zero, One, Many]
def ZO := any[Zero, One]
def zero implements ZeroStamp {}
def one(value) { return &value }
def many implements ManyStamp {}

def makeLamportSlot := <import:org.erights.e.elib.slot.makeLamportSlot>
def toKey                    := <elib:tables.makeTraversalKey>
def FinalSlot := <type:org.erights.e.elib.slot.FinalSlot>

def CompleteCommand
interface Command {
  to snapshot(partialEj) :CompleteCommand
}

interface Presenter {
  to run(ref :any, context) :any
}

interface bind CompleteCommand extends Command {
  to run() :Tuple[any, nullOk[Presenter]]
}

def CommandMessageDesc := <type:org.erights.e.elib.base.MessageDesc> # XXX should be just DeepFrozen and an interface

def makeZeroSlots(n) { 
  return accum [] for _ in (0..!n) { _.with(makeLamportSlot(zero)) } 
}

interface SimpleMessageCommand guards SimpleMessageCommandStamp {}
def makeSimpleMessageCommand(recipient :any, verb :any, args :List[any]) {
  return def simpleMessageCommand implements Command, CompleteCommand, SimpleMessageCommandStamp {
    to getRecipient() { return recipient }
    to getVerb() { return verb }
    to getArgs() { return args }
    to snapshot() { return simpleMessageCommand }
    to run() { return [E.send(recipient, verb, args), null] }
  }
}

interface FlexArgMessageCommand guards FlexArgMessageCommandStamp {}
def makeFlexArgMessageCommand {
  to run(recipient :any, messageDesc :CommandMessageDesc, argZoSlots :List) {
    require(messageDesc.getParams().size() == argZoSlots.size())
    return def flexArgMessageCommand implements Command, FlexArgMessageCommandStamp {
      to getRecipient() { return recipient }
      to getVerb() { return messageDesc.getVerb() }
      to getParams() { return messageDesc.getParams() }
      to getArgZoSlots() { return argZoSlots }
      to run() {
        return flexArgMessageCommand.snapshot(null).run()
      }
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
    return makeFlexArgMessageCommand(r, d, makeZeroSlots(d.getParams().size()))
  }
}

def makeArglessMessageCommand(recipient :any, messageDesc :CommandMessageDesc) {
  return if (messageDesc.getParams().size() > 0) {
    makeSimpleMessageCommand(makeFlexArgMessageCommand, "empty", [recipient, messageDesc])
  } else {
    makeSimpleMessageCommand(recipient, messageDesc.getVerb(), [])
  }
}

def makePresentationContext(makePresentationBoundary, present, seen, quoting, hooks) {
  return def presentationContext {
    to quoting() { return quoting }
    to getHooks() { return hooks } # XXX in the ideal system this should not exist
    
    to subPresent(object, quoting) {
      return presentationContext.subPresentType(object, present, quoting)
    }
    
    to subPresentType(object, type, quoting) {
      def key := toKey(object)
      def hooks
      def subC := makePresentationContext(makePresentationBoundary, present, seen.with(key), quoting, hooks)
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

if (currentVat.getRunnerKind() != "awt") {
  interp.waitAtTop(currentVat.morphInto("awt"))
}

def makeJLabel := <swing:makeJLabel>
def makeJTextField := <swing:makeJTextField>
def borderFactory := <swing:makeBorderFactory>
def dragDropKit := <import:com.skyhunter.e.awt.dnd.dragDropKit>(<awt>, def _(_) {})
def awtDropTarget := <awt:dnd.makeDropTarget>
def swingConstants := <swing:makeSwingConstants>

def action := <aui:swing.action>
def attachContextMenu := <aui:swing.attachContextMenu>
def makeGridBagTable := <aui:swing.makeGridBagTable>

def makeObjectSelector := <aui:swing.makeObjectSelectorAuthor>(one, zero, awtDropTarget)

def presentInSwing
def presentInSwingIcon
def makeIconContext

def rootContext

def openSwingFrame(title, component, optRel) {
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

def enbox {
  #match [dn ? via (["x" => 0, "y" => 1].fetch) dir, components]
  match [dn ? (["x" => 0, "y" => 1] =~ [(dn) => dir]|_), components] {
    def box := <swing:makeBox>(dir)
    for c in components { 
      box."add(Component)"(c) 
    }
    box
  }
}

# ------------------------------------------------------------------------------

def presentFAMCommandInSwing
def presentAMCommandAsSwingMenuItem
def runToWindow

def addStandardEventHandlers(component, object, context) {
  dragDropKit.setupLocalDragSource(component, thunk { object })

  attachContextMenu(component, thunk {
    def m := <swing:JPopupMenu>()
    
    when (gatherCommands(object)) -> doCommandMenu(commands) {
      for command in commands {
        m."add(Component)"(presentAMCommandAsSwingMenuItem(command, context, object))
      }

      m.addPopupMenuListener(context.getHooks()["borderControlListener"])

      return m
    } catch p {
      <awt:Toolkit>.getDefaultToolkit().beep() # besides the tracelog
      throw <- (p)
      return m
    }
  })
}

def makePLabel(name :String, icon, context, object, getDoubleClickCommand) {
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

def textLimit := 60

def presentGenericInSwing(object, context) {
  def quoted := context.quoting()

  def label := makePLabel("", presentInSwingIcon(object, makeIconContext(context)), context, object, thunk {thunk {[null,null]}})

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
  def runButton := <swing:makeJButton>("Run")
  def hole := JPanel``
  hole.setLayout(<awt:FlowLayout>())
  #hole.setPreferredSize(<awt:Dimension>(320, 100))

  action(runButton, thunk {
    #hole.setPreferredSize(null)
    hole."add(Component)"(context.subPresent(command.run(), true))
    hole.revalidate()
  })

  return JPanel`
    ${enbox.x(JPanel``, runButton)}.X
    $hole.X.Y
  `
}

def makeCommandUI(command :Command, editUI, context) {
  def runButton := <swing:makeJButton>("Run")
  def hole := JPanel``
  hole.setLayout(<awt:FlowLayout>())
  hole.setPreferredSize(<awt:Dimension>(320, 100))

  action(runButton, thunk {
    hole.setPreferredSize(null)
    hole."add(Component)"(context.subPresent(command.run(), true))
    hole.revalidate()
  })

  return JPanel`
    $editUI.X
    ${enbox.x(JPanel``, runButton)}.X
    $hole.Y
  `
}

bind presentFAMCommandInSwing(command, context) {
  def descs := command.getParams()

  def selsC := <swing:Box>(1)
  for i => zos in command.getArgZoSlots() { 
    selsC."add(Component)"(enbox.x(
      context.subPresent(descs[i], false),
      context.subPresentType(zos, makeObjectSelector, true),
    )) 
  }

  def leftLabel := JPanel`${context.subPresent(command.getRecipient(), true)} ${makeJLabel(" <- ")} ${context.subPresent(command.getVerb(), false)} ${makeJLabel("(")}`

  return makeCommandUI(command, JPanel`
    $leftLabel $selsC ${makeJLabel(")")}
  `, context)
}

/** demo thing */
def presentMakeFlexMapCommandInSwing(command, context) {
  
  def [kzs, vzs] := command.getArgZoSlots()
  def ksel := context.subPresentType(kzs, makeObjectSelector, false)
  def vsel := context.subPresentType(vzs, makeObjectSelector, false)

  return makeCommandUI(command, JPanel`
    ${makeJLabel("Key type: ")}   $ksel.X
    ${makeJLabel("Value type: ")} $vsel.X
  `, context)
}

def presentListY(list, context) {
  def box := E.call(enbox, "y", accum [] for item in list { _.with(context.subPresent(item, context.quoting())) })
  return box
}
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

  def hole := enbox.x(placeholder)
  
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
    #  makeJLabel(x) }
      
    # broken - not all lamports hold ZOMs
    #match ls :near ? (ls.__getAllegedType().getFQName() =~ `org.erights.e.elib.slot.makeLamportSlot$$makeLamportSlot__C$$lamportSlot__@{_}__C`) { 
    #  makeObjectSelector(ls, context) }
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
  def context := rootContext
  def [result, optPresenter] := command.run()
  openSwingFrame(title, 
                 if (optPresenter != null) \
                   {context.subPresentType(result, optPresenter, true)} else \
                   {context.subPresent(result, true)}, 
                 originC)
}

bind presentAMCommandAsSwingMenuItem(command :CompleteCommand, context, selected) {
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

  def mi := <swing:JMenuItem>(commandLabel)
  action(mi, thunk {
    runToWindow(resultLabelTh(), command, mi)
  })

  return mi
}




def boundaryBorder := borderFactory.createRaisedBevelBorder()
def activeBorder := borderFactory.createBevelBorder(
  <swing:border.BevelBorder>.getRAISED(),
  <awt:Color>."run(float, float, float)"(0.7, 0.7, 1.0),
  <awt:Color>."run(float, float, float)"(0.4, 0.4, 0.7))

def makeSwingBoundary(object, context, content) {
  def container := JPanel`$content`

  addStandardEventHandlers(container, object, context)

  def normalBorder := if (context.quoting()) {
    boundaryBorder
  } else {
    borderFactory.createEmptyBorder()
  }
  container.setBorder(normalBorder)

  def borderControlListener {
    to popupMenuWillBecomeInvisible(e) { try { 
      container.setBorder(normalBorder)
    } catch p { throw <- (p) } }
    to popupMenuWillBecomeVisible(e) { try {
        container.setBorder(activeBorder)
    } catch p { throw <- (p) } }
    match msg {}
  }

  return [container, [=> borderControlListener]]
}

bind rootContext := makePresentationContext(makeSwingBoundary, presentInSwing, [].asSet(), true, Ref.broken("no hooks"))

# ------------------------------------------------------------------------------

# Icons

def genericIcon := <swing:makeImageIcon>(<resource:org/cubik/cle/aui/swing/item.gif>)
def fileIcon := <swing:makeImageIcon>(<resource:com/skyhunter/capDesk/icons/noLauncher.gif>)
def dirIcon := <swing:makeImageIcon>(<resource:com/skyhunter/capDesk/icons/folder.gif>)

def File := <type:java.io.File>

bind makeIconContext(context) {
  return context
}

bind presentInSwingIcon(object, context) {
  return switch (object) {
    match f :File {
      f.isDirectory().pick(dirIcon, fileIcon)
    }
    match _ { genericIcon }
  }
}

# ------------------------------------------------------------------------------

def parse := e__quasiParser

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
          match =="DROP_TARGET" { return awtDropTarget } # XXX what authority is this, anyway? why does the powerbox pass it out freely, and yet it isn't in the safeScope?
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
        
        def fsel := rootContext.subPresentType(fileSlot, makeObjectSelector, false)
      
        def ui := JPanel`
          ${makeJLabel(`Justification: "$justification"`)}
          $fsel >
          ${JPanel``}.X ${def runButton := <swing:makeJButton>("Run")}
        `

        def context := rootContext
        def frame := openSwingFrame(`CapDesk - $capletName requests ${editable.pick("editable", "read-only")} file: $title`,
                                    ui,
                                    container)
        
        action(runButton, thunk {
          bind files := [fileSlot.getValue().getValue()]
          frame.hide()
        })
        
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
    return makePLabel(name, presentInSwingIcon(file, makeIconContext(context)), context, file, thunk { makeOpenCommand(file) })
  }
}

def presentDirectory(dir, context) {
  def container := enbox.y()
  for name => file in dir {
    container."add(Component)"(context.subPresentType(file, presentDirEntry(name), true))
  }
  return container
}

interface OpenCommand guards OpenCommandStamp {}
bind makeOpenCommand(file) {
  return def openCommand implements Command, CompleteCommand, OpenCommandStamp {
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

openSwingFrame("File Browser", rootContext.subPresentType(exampleFile, presentDirectory, true), null) 

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

def VK_ENTER := <awt:event.KeyEvent>.getVK_ENTER()

def presentSeqEval(seqEval, context) {
  def box := enbox.y(def filler := JPanel``)
  filler.setPreferredSize(<awt:Dimension>(320,320))
  def read() {
    box."add(Component)"(def field := makeJTextField())
    field.addKeyListener(def enterKeyListener {
      to keyPressed(e) :void { try {
        if (e.getKeyCode() == VK_ENTER) {
          def expr := e__quasiParser(field.getText())
          box.remove(field)
          box.add(JPanel`${makeJLabel("? ")} ${context.subPresent(expr, false)} ${JPanel``}`)
          box.add(context.subPresent(seqEval <- (expr), true))
          box.revalidate()
          read <- ()
        }
      } catch p { throw <- (p) } }
      match _ {}
    })
  }
  read()
  return box
}

openSwingFrame("Repl", rootContext.subPresentType(seqEval, presentSeqEval, false), null) 

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
  => openSwingFrame, 
  => privilegedScope, 
  => safeScope,
]

# disabled for now as the REPL incorporates this mostly
# openSwingFrame("Toy", makePresentationContext(makeSwingBoundary, presentInSwing, [].asSet(), true, [].asMap()).subPresent(example, true), null) 
