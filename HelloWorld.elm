import Preselm (..)

frame1 = { emptyFrame | middle <- Just [markdown|# Hello World!|] }

frame2 = { emptyFrame | title <- Just "Goodbye", middle <- Just [markdown|#World!|] }

main = presentation [frame1, frame2]
