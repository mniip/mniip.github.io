Jekyll::Hooks.register :site, :post_write do |site|
  result = system(
    "node",
    File.join(site.source, "_syntax-highlight", "highlight.mjs"),
    site.dest
  )
  if !result
    raise "Syntax highlighting failed"
  end
end
