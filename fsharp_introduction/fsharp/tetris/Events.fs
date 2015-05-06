module Events

open VisualTree

type RenderEvent =
  | Game          of MailboxProcessor<GameEvent>
  | NewVisualTree of VisualTree

and GameEvent =
  | Tick
  | BlockRotate
  | BlockDrop
  | BlockDown
  | BlockLeft
  | BlockRight

