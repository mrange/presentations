module Events

open VisualTree

type RenderEvent =
  | Game          of MailboxProcessor<GameEvent>
  | NewVisualTree of VisualTree

and GameEvent =
  | Tick
  | KeyDown
  | KeyUp
  | KeyLeft
  | KeyRight

