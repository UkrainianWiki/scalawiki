package org.scalawiki.dto.markup

import org.sweble.wikitext.engine.utils.DefaultConfigEnWp
import org.sweble.wikitext.parser.nodes.WikitextNodeFactoryImpl

object NodeFactory extends WikitextNodeFactoryImpl(DefaultConfigEnWp.generate.getParserConfig)
