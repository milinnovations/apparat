/*
 * This file is part of Apparat.
 *
 * Copyright (C) 2010 Joa Ebert
 * http://www.joa-ebert.com/
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */
package apparat.graph.mutable

import apparat.graph._
import analysis.{StronglyConnectedComponentFinder, Dominance}

trait MutableGraphLike[V] extends GraphLike[V] {
	def +=(that: (V, V))(implicit f: (V, V) => E): Unit = {
		if (!contains(that._1)) add(that._1)
		if (!contains(that._2)) add(that._2)
		add(f(that._1, that._2))
	}

	def -=(that: (V, V)): Unit = {
		if (contains(that._1) && contains(that._2))
			outgoingOf(that._1) filter (_.endVertex == that._2) foreach remove _
	}

	def +=(that: E): Unit = add(that)

	def +=(that: V): Unit = add(that)

	def -=(that: E): Unit = remove(that)

	def -=(that: V): Unit = remove(that)

	def ++=(that: Traversable[V]): Unit = that foreach add

	override def dominance = new Dominance(this)

	override def topsort = new TopsortTraversal(this)

	override def sccs = new StronglyConnectedComponentFinder(this)

	override def indegreeOf(vertex: V) = incomingOf(vertex).iterator.length

	override def outdegreeOf(vertex: V) = outgoingOf(vertex).iterator.length

	override def predecessorsOf(vertex: V) = incomingOf(vertex) map (_.startVertex)

	override def successorsOf(vertex: V) = outgoingOf(vertex) map (_.endVertex)

	def add(edge: E): Unit

	def remove(edge: E): Unit

	def add(vertex: V): Unit

	def remove(vertex: V): Unit
}
