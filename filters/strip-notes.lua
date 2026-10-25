-- Remove reveal.js speaker notes from a published deck.
--
-- Notes written with `::: {.notes}` are embedded in the slide HTML, where any
-- visitor can read them by pressing "s" or viewing the source. They routinely
-- hold things meant only for the speaker: who is in the room, unpublished or
-- undecided funding, reminders about what not to claim. So the deployed site
-- carries slides only.
--
-- Local renders keep the notes, so the deck is ready to present from:
--   quarto render presentations/<dir>/index.qmd
--
-- Notes are dropped when the render runs on Netlify (which sets NETLIFY=true
-- in its builds), or on request:
--   quarto render presentations/<dir>/index.qmd -M strip-notes:true
--
-- Two filter tables, so the metadata is read in the first pass and the notes
-- are removed in the second. A single table would visit Meta last, after the
-- divs had already been decided.

local strip = os.getenv("NETLIFY") == "true"

return {
  {
    Meta = function(meta)
      if meta["strip-notes"] == true then strip = true end
      return nil
    end
  },
  {
    Div = function(el)
      if strip and el.classes:includes("notes") then
        return {}
      end
    end
  }
}
