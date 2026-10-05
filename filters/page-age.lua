-- Add "Last updated" with the date of the page source's last git commit to the
-- foot of each page, so a stale page shows its age. Styled by .page-age.
--
-- The date comes from `git log` at render time. If git is unavailable, or the
-- file has no commits yet (a new, uncommitted page), nothing is added.
-- A page can opt out with `page-age: false` in its front matter.

local months = { "January", "February", "March", "April", "May", "June", "July",
                 "August", "September", "October", "November", "December" }

local function last_commit_date(path)
  local cmd = string.format('git log -1 --format=%%cs -- "%s"', path)
  local handle = io.popen(cmd)
  if not handle then return nil end
  local out = handle:read("*a") or ""
  handle:close()
  local y, m, d = out:match("(%d%d%d%d)%-(%d%d)%-(%d%d)")
  if not y then return nil end
  return string.format("%d %s %s", tonumber(d), months[tonumber(m)], y)
end

function Pandoc(doc)
  if not quarto.doc.is_format("html") then return doc end
  if doc.meta["page-age"] == false then return doc end
  local input = quarto.doc.input_file
  if not input then return doc end
  local date = last_commit_date(input)
  if not date then return doc end
  local note = pandoc.Div(pandoc.Para({ pandoc.Str("Last updated " .. date) }), { class = "page-age" })
  doc.blocks:insert(note)
  return doc
end
